import Foundation
import OrgRemCore

private struct NotImplementedStore: ReminderStore {
    func list(listID: String) throws -> (ReminderListRecord, [ReminderRecord]) {
        throw NSError(
            domain: "orgrem",
            code: 1,
            userInfo: [NSLocalizedDescriptionKey: "EventKit adapter not implemented yet."]
        )
    }

    func apply(listID: String, request: ApplyRequest) throws -> ApplyResponse {
        throw NSError(
            domain: "orgrem",
            code: 1,
            userInfo: [NSLocalizedDescriptionKey: "EventKit adapter not implemented yet."]
        )
    }
}

@main
struct OrgRemCLI {
    static func main() {
        do {
            let output = try Runner.run(
                args: Array(CommandLine.arguments.dropFirst()),
                store: NotImplementedStore()
            )
            print(output)
        } catch {
            fputs("\(error)\n", stderr)
            exit(2)
        }
    }
}
