import Foundation
import OrgRemCore
import Testing

#if canImport(EventKit)
import EventKit

private enum IntegrationError: Error, CustomStringConvertible {
    case missingEnv(String)
    case remindersAccessDenied
    case noReminderSource
    case cliFailed(Int32, String)
    case missingItem(String)
    case parseFailure(String)

    var description: String {
        switch self {
        case .missingEnv(let key):
            return "Missing environment variable: \(key)"
        case .remindersAccessDenied:
            return "Reminders access denied for integration tests."
        case .noReminderSource:
            return "Unable to resolve a reminders source for temporary list creation."
        case .cliFailed(let code, let stderr):
            return "orgrem failed (\(code)): \(stderr)"
        case .missingItem(let message):
            return "Missing expected item: \(message)"
        case .parseFailure(let message):
            return "Parse failure: \(message)"
        }
    }
}

private func integrationEnabled() -> Bool {
    ProcessInfo.processInfo.environment["ORGREM_RUN_INTEGRATION"] == "1"
}

private func requiredEnv(_ key: String) throws -> String {
    guard let value = ProcessInfo.processInfo.environment[key],
          !value.isEmpty else {
        throw IntegrationError.missingEnv(key)
    }
    return value
}

@discardableResult
private func runProcess(
    executable: String,
    args: [String],
    env: [String: String] = [:],
    cwd: String? = nil
) throws -> (stdout: String, stderr: String) {
    let process = Process()
    process.executableURL = URL(fileURLWithPath: executable)
    process.arguments = args
    if let cwd {
        process.currentDirectoryURL = URL(fileURLWithPath: cwd, isDirectory: true)
    }
    if !env.isEmpty {
        var merged = ProcessInfo.processInfo.environment
        env.forEach { merged[$0.key] = $0.value }
        process.environment = merged
    }

    let stdout = Pipe()
    let stderr = Pipe()
    process.standardOutput = stdout
    process.standardError = stderr

    try process.run()
    process.waitUntilExit()

    let stdoutText = String(decoding: stdout.fileHandleForReading.readDataToEndOfFile(), as: UTF8.self)
    let stderrText = String(decoding: stderr.fileHandleForReading.readDataToEndOfFile(), as: UTF8.self)
    guard process.terminationStatus == 0 else {
        throw IntegrationError.cliFailed(process.terminationStatus, stderrText)
    }
    return (stdoutText, stderrText)
}

private func runCLI(bin: String, args: [String]) throws -> String {
    try runProcess(executable: bin, args: args).stdout
}

private func runList(bin: String, listID: String) throws -> ListResponse {
    let json = try runCLI(bin: bin, args: ["list", "--list-id", listID])
    return try JSONDecoder().decode(ListResponse.self, from: Data(json.utf8))
}

private func runApply(bin: String, listID: String, ops: [ApplyOp]) throws -> ApplyResponse {
    let request = ApplyRequest(schemaVersion: 1, ops: ops)
    let encoder = JSONEncoder()
    encoder.outputFormatting = [.sortedKeys]
    let payload = try encoder.encode(request)

    let opsURL = URL(fileURLWithPath: NSTemporaryDirectory())
        .appendingPathComponent("orgrem-integration-ops-\(UUID().uuidString).json")
    try payload.write(to: opsURL)
    defer { try? FileManager.default.removeItem(at: opsURL) }

    let json = try runCLI(
        bin: bin,
        args: ["apply", "--list-id", listID, "--ops-file", opsURL.path]
    )
    return try JSONDecoder().decode(ApplyResponse.self, from: Data(json.utf8))
}

private func elispStringLiteral(_ raw: String) -> String {
    raw.replacingOccurrences(of: "\\", with: "\\\\")
        .replacingOccurrences(of: "\"", with: "\\\"")
}

private func runEmacsSync(
    repoRoot: String,
    configPath: String,
    emacsBin: String
) throws {
    let escapedConfig = elispStringLiteral(configPath)
    let form = """
    (progn
      (setq org-agenda-files nil)
      (setq org-agenda-skip-unavailable-files t)
      (require 'sync-cli)
      (org-rem-sync-run "\(escapedConfig)"))
    """
    _ = try runProcess(
        executable: "/usr/bin/env",
        args: [
            emacsBin,
            "--batch",
            "-Q",
            "-L", "\(repoRoot)/sync/elisp",
            "--eval", form,
        ],
        cwd: repoRoot
    )
}

private func readMappingCount(
    repoRoot: String,
    dbPath: String,
    emacsBin: String
) throws -> Int {
    let escapedDB = elispStringLiteral(dbPath)
    let form = """
    (progn
      (require 'sync-db)
      (let ((db (org-rem-db-open "\(escapedDB)")))
        (unwind-protect
            (progn
              (org-rem-db-init db)
              (princ (length (org-rem-db-list-mappings db))))
          (org-rem-db-close db))))
    """
    let out = try runProcess(
        executable: "/usr/bin/env",
        args: [
            emacsBin,
            "--batch",
            "-Q",
            "-L", "\(repoRoot)/sync/elisp",
            "--eval", form,
        ],
        cwd: repoRoot
    ).stdout.trimmingCharacters(in: .whitespacesAndNewlines)
    guard let count = Int(out) else {
        throw IntegrationError.parseFailure("Unable to parse mapping count from: \(out)")
    }
    return count
}

private func writeSyncConfig(
    path: String,
    orgRoot: String,
    inboxFile: String,
    dbPath: String,
    listID: String,
    orgremBin: String
) throws {
    let content = """
    (:org-root "\(elispStringLiteral(orgRoot))"
     :inbox-file "\(elispStringLiteral(inboxFile))"
     :inbox-heading "* Reminders"
     :db-path "\(elispStringLiteral(dbPath))"
     :reminders-list-id "\(elispStringLiteral(listID))"
     :orgrem-bin "\(elispStringLiteral(orgremBin))")
    """
    try content.write(to: URL(fileURLWithPath: path), atomically: true, encoding: .utf8)
}

private func ensureRemindersAccess(_ store: EKEventStore) async throws {
    let status = EKEventStore.authorizationStatus(for: .reminder)
    switch status {
    case .fullAccess:
        return
    case .notDetermined:
        let granted: Bool
        if #available(macOS 14.0, *) {
            granted = try await withCheckedThrowingContinuation { continuation in
                store.requestFullAccessToReminders { ok, error in
                    if let error {
                        continuation.resume(throwing: error)
                    } else {
                        continuation.resume(returning: ok)
                    }
                }
            }
        } else {
            granted = try await withCheckedThrowingContinuation { continuation in
                store.requestAccess(to: .reminder) { ok, error in
                    if let error {
                        continuation.resume(throwing: error)
                    } else {
                        continuation.resume(returning: ok)
                    }
                }
            }
        }
        guard granted else { throw IntegrationError.remindersAccessDenied }
    case .writeOnly, .denied, .restricted:
        throw IntegrationError.remindersAccessDenied
    @unknown default:
        throw IntegrationError.remindersAccessDenied
    }
}

private func createTemporaryList(_ store: EKEventStore) throws -> EKCalendar {
    let calendar = EKCalendar(for: .reminder, eventStore: store)
    let source = store.defaultCalendarForNewReminders()?.source
        ?? store.sources.first(where: { $0.sourceType == .local })
        ?? store.sources.first
    guard let source else {
        throw IntegrationError.noReminderSource
    }
    calendar.source = source
    calendar.title = "orgrem-integration-\(UUID().uuidString)"
    try store.saveCalendar(calendar, commit: true)
    return calendar
}

private func createReminder(
    _ store: EKEventStore,
    calendar: EKCalendar,
    title: String,
    notes: String?
) throws {
    let reminder = EKReminder(eventStore: store)
    reminder.calendar = calendar
    reminder.title = title
    reminder.notes = notes
    reminder.isCompleted = false
    try store.save(reminder, commit: true)
}

@Test func liveListAndApplyRoundTrip() async throws {
    guard integrationEnabled() else { return }

    let orgremBin = try requiredEnv("ORGREM_INTEGRATION_BIN")
    let store = EKEventStore()
    try await ensureRemindersAccess(store)

    let list = try createTemporaryList(store)
    defer { try? store.removeCalendar(list, commit: true) }

    let fixtureTitle = "fixture-\(UUID().uuidString)"
    try createReminder(store, calendar: list, title: fixtureTitle, notes: "fixture")

    let listed = try runList(bin: orgremBin, listID: list.calendarIdentifier)
    #expect(listed.items.contains(where: { $0.title == fixtureTitle }))

    let create = ApplyOp(
        op: .create,
        clientRef: "new-client-ref",
        externalID: nil,
        ifLastModified: nil,
        fields: ApplyOpFields(
            title: "created-via-apply #tag",
            notes: "created-notes",
            completed: false,
            start: nil,
            due: nil,
            url: nil
        )
    )
    let createResponse = try runApply(bin: orgremBin, listID: list.calendarIdentifier, ops: [create])
    #expect(createResponse.results.count == 1)
    #expect(createResponse.results[0].status == .ok)
    guard let createdID = createResponse.results[0].item?.externalID else {
        throw IntegrationError.missingItem("created external_id")
    }

    let listedAfterCreate = try runList(bin: orgremBin, listID: list.calendarIdentifier)
    guard let createdItem = listedAfterCreate.items.first(where: { $0.externalID == createdID }) else {
        throw IntegrationError.missingItem("created reminder \(createdID)")
    }
    #expect(createdItem.notes == "created-notes")

    let update = ApplyOp(
        op: .update,
        clientRef: nil,
        externalID: createdID,
        ifLastModified: createdItem.lastModified,
        fields: ApplyOpFields(
            title: "updated-via-apply #tag",
            notes: "updated-notes",
            completed: true,
            start: nil,
            due: nil,
            url: nil
        )
    )
    let updateResponse = try runApply(bin: orgremBin, listID: list.calendarIdentifier, ops: [update])
    #expect(updateResponse.results.count == 1)
    #expect(updateResponse.results[0].status == .ok)

    let listedAfterUpdate = try runList(bin: orgremBin, listID: list.calendarIdentifier)
    guard let updatedItem = listedAfterUpdate.items.first(where: { $0.externalID == createdID }) else {
        throw IntegrationError.missingItem("updated reminder \(createdID)")
    }
    #expect(updatedItem.title == "updated-via-apply #tag")
    #expect(updatedItem.completed)

    let delete = ApplyOp(
        op: .delete,
        clientRef: nil,
        externalID: createdID,
        ifLastModified: updatedItem.lastModified,
        fields: nil
    )
    let deleteResponse = try runApply(bin: orgremBin, listID: list.calendarIdentifier, ops: [delete])
    #expect(deleteResponse.results.count == 1)
    #expect(deleteResponse.results[0].status == .ok)

    let listedAfterDelete = try runList(bin: orgremBin, listID: list.calendarIdentifier)
    #expect(!listedAfterDelete.items.contains(where: { $0.externalID == createdID }))
}

@Test func liveEndToEndSyncOrgReminderAndDB() async throws {
    guard integrationEnabled() else { return }

    let orgremBin = try requiredEnv("ORGREM_INTEGRATION_BIN")
    let repoRoot = try requiredEnv("ORGREM_REPO_ROOT")
    let emacsBin = ProcessInfo.processInfo.environment["ORGREM_INTEGRATION_EMACS"] ?? "emacs"
    let store = EKEventStore()
    try await ensureRemindersAccess(store)

    let list = try createTemporaryList(store)
    defer { try? store.removeCalendar(list, commit: true) }

    let tmp = URL(fileURLWithPath: NSTemporaryDirectory())
        .appendingPathComponent("orgrem-e2e-\(UUID().uuidString)", isDirectory: true)
    try FileManager.default.createDirectory(at: tmp, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: tmp) }

    let orgRoot = tmp.appendingPathComponent("org", isDirectory: true)
    try FileManager.default.createDirectory(at: orgRoot, withIntermediateDirectories: true)
    let inboxFile = orgRoot.appendingPathComponent("inbox.org")
    let tasksFile = orgRoot.appendingPathComponent("tasks.org")
    let dbPath = tmp.appendingPathComponent("sync.sqlite").path
    let configPath = tmp.appendingPathComponent("sync-config.el").path
    try writeSyncConfig(
        path: configPath,
        orgRoot: orgRoot.path,
        inboxFile: inboxFile.path,
        dbPath: dbPath,
        listID: list.calendarIdentifier,
        orgremBin: orgremBin
    )

    let reminderTitle = "phone-task-\(UUID().uuidString) #home"
    try createReminder(store, calendar: list, title: reminderTitle, notes: "from-reminders")
    try runEmacsSync(repoRoot: repoRoot, configPath: configPath, emacsBin: emacsBin)

    let inboxText = try String(contentsOf: inboxFile, encoding: .utf8)
    #expect(inboxText.contains("phone-task-"))
    #expect(inboxText.contains("from-reminders"))

    let orgTitle = "org-task-\(UUID().uuidString)"
    let orgBody = "from-org-body"
    let orgFixture = "* TODO \(orgTitle) :work:\n\(orgBody)\n"
    try orgFixture.write(to: tasksFile, atomically: true, encoding: .utf8)
    try runEmacsSync(repoRoot: repoRoot, configPath: configPath, emacsBin: emacsBin)

    let listed = try runList(bin: orgremBin, listID: list.calendarIdentifier)
    #expect(listed.items.contains(where: { $0.title.contains(orgTitle) }))
    #expect(listed.items.contains(where: { $0.notes == orgBody }))

    let mappingCount = try readMappingCount(repoRoot: repoRoot, dbPath: dbPath, emacsBin: emacsBin)
    #expect(mappingCount >= 2)
}

#else

@Test func liveListAndApplyRoundTripUnavailablePlatform() {
    // This integration suite requires EventKit (macOS).
}

#endif
