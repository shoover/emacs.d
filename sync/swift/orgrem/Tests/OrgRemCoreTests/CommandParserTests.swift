import Testing
@testable import OrgRemCore

@Test func parsesListCommand() throws {
    let command = try CommandParser.parse(["list", "--list-id", "LIST1"])
    #expect(command == .list(listID: "LIST1"))
}

@Test func parsesApplyCommand() throws {
    let command = try CommandParser.parse([
        "apply",
        "--list-id",
        "LIST1",
        "--ops-file",
        "/tmp/ops.json",
    ])
    #expect(command == .apply(listID: "LIST1", opsFile: "/tmp/ops.json"))
}

@Test func parseRejectsMissingFlagValue() {
    #expect(throws: CommandParseError.self) {
        _ = try CommandParser.parse(["list", "--list-id"])
    }
}
