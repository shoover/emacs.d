import Foundation

public protocol ReminderStore {
    func list(listID: String) throws -> (ReminderListRecord, [ReminderRecord])
    func apply(listID: String, request: ApplyRequest) throws -> ApplyResponse
}

public enum Runner {
    public static func run(
        args: [String],
        store: ReminderStore,
        nowISO8601: () -> String = { ISO8601DateFormatter().string(from: Date()) }
    ) throws -> String {
        let command = try CommandParser.parse(args)
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.sortedKeys]

        switch command {
        case .list(let listID):
            let (list, items) = try store.list(listID: listID)
            let payload = ListResponse(
                schemaVersion: 1,
                generatedAt: nowISO8601(),
                list: list,
                items: items
            )
            return String(decoding: try encoder.encode(payload), as: UTF8.self)
        case .apply(let listID, let opsFile):
            let data = try Data(contentsOf: URL(fileURLWithPath: opsFile))
            let request = try JSONDecoder().decode(ApplyRequest.self, from: data)
            let response = try store.apply(listID: listID, request: request)
            return String(decoding: try encoder.encode(response), as: UTF8.self)
        }
    }
}
