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

private func runCLI(bin: String, args: [String]) throws -> String {
    let process = Process()
    process.executableURL = URL(fileURLWithPath: bin)
    process.arguments = args

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
    return stdoutText
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

#else

@Test func liveListAndApplyRoundTripUnavailablePlatform() {
    // This integration suite requires EventKit (macOS).
}

#endif
