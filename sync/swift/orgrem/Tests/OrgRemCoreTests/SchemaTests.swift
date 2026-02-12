import Foundation
import Testing
@testable import OrgRemCore

@Test func listResponseEncodesSchemaVersionAndItemFields() throws {
    let item = ReminderRecord(
        externalID: "x-apple-reminder://abc",
        localID: "LOCAL123",
        title: "Pay rent #finance #home",
        notes: "verbatim",
        completed: false,
        completionDate: nil,
        start: nil,
        due: ReminderDateComponents(year: 2026, month: 2, day: 15, hour: nil, minute: nil, timeZone: nil),
        url: "https://example.com",
        lastModified: "2026-02-12T21:35:18Z"
    )
    let payload = ListResponse(
        schemaVersion: 1,
        generatedAt: "2026-02-12T21:40:00Z",
        list: ReminderListRecord(id: "LIST1", title: "Personal"),
        items: [item]
    )

    let encoder = JSONEncoder()
    encoder.outputFormatting = [.sortedKeys]
    let data = try encoder.encode(payload)
    let text = String(decoding: data, as: UTF8.self)
    let decoded = try JSONDecoder().decode(ListResponse.self, from: data)

    #expect(text.contains("\"schema_version\":1"))
    #expect(decoded.items.first?.externalID == "x-apple-reminder://abc")
    #expect(decoded.items.first?.lastModified == "2026-02-12T21:35:18Z")
}

@Test func applyRequestDecodesOps() throws {
    let json = """
    {
      "schema_version": 1,
      "ops": [
        {
          "op": "delete",
          "external_id": "x-apple-reminder://abc",
          "if_last_modified": "2026-02-12T21:35:18Z"
        }
      ]
    }
    """

    let request = try JSONDecoder().decode(
        ApplyRequest.self,
        from: Data(json.utf8)
    )

    #expect(request.schemaVersion == 1)
    #expect(request.ops.count == 1)
    #expect(request.ops[0].op == .delete)
    #expect(request.ops[0].externalID == "x-apple-reminder://abc")
}
