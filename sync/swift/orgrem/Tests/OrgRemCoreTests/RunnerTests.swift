import Foundation
import Testing
@testable import OrgRemCore

private final class MockStore: ReminderStore {
    var listedListID: String?
    var appliedListID: String?
    var appliedRequest: ApplyRequest?

    var listItems: [ReminderRecord] = []
    var listRecord = ReminderListRecord(id: "LIST1", title: "Personal")
    var applyResponse = ApplyResponse(schemaVersion: 1, appliedAt: "2026-02-12T21:40:30Z", results: [])

    func list(listID: String) throws -> (ReminderListRecord, [ReminderRecord]) {
        listedListID = listID
        return (listRecord, listItems)
    }

    func apply(listID: String, request: ApplyRequest) throws -> ApplyResponse {
        appliedListID = listID
        appliedRequest = request
        return applyResponse
    }
}

@Test func runnerExecutesListAndReturnsJSON() throws {
    let store = MockStore()
    store.listItems = [
        ReminderRecord(
            externalID: "x-apple-reminder://abc",
            localID: "LOCAL123",
            title: "Pay rent #finance #home",
            notes: nil,
            completed: false,
            completionDate: nil,
            start: nil,
            due: nil,
            url: nil,
            lastModified: "2026-02-12T21:35:18Z"
        )
    ]

    let output = try Runner.run(
        args: ["list", "--list-id", "LIST1"],
        store: store,
        nowISO8601: { "2026-02-12T21:40:00Z" }
    )

    #expect(store.listedListID == "LIST1")
    #expect(output.contains("\"schema_version\":1"))
    #expect(output.contains("\"generated_at\":\"2026-02-12T21:40:00Z\""))
}

@Test func runnerExecutesApplyFromOpsFile() throws {
    let store = MockStore()
    let path = URL(fileURLWithPath: NSTemporaryDirectory())
        .appendingPathComponent("orgrem-ops-\(UUID().uuidString).json").path
    defer { try? FileManager.default.removeItem(atPath: path) }

    let ops = """
    {
      "schema_version": 1,
      "ops": [
        {
          "op": "delete",
          "external_id": "x-apple-reminder://abc"
        }
      ]
    }
    """
    try ops.write(toFile: path, atomically: true, encoding: .utf8)

    _ = try Runner.run(
        args: ["apply", "--list-id", "LIST1", "--ops-file", path],
        store: store,
        nowISO8601: { "2026-02-12T21:40:00Z" }
    )

    #expect(store.appliedListID == "LIST1")
    #expect(store.appliedRequest?.ops.count == 1)
    #expect(store.appliedRequest?.ops.first?.op == .delete)
}
