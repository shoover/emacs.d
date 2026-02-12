import Foundation
import OrgRemCore

@main
struct OrgRemCLI {
    static func main() {
        do {
            let command = try CommandParser.parse(Array(CommandLine.arguments.dropFirst()))
            switch command {
            case .list(let listID):
                print("list not implemented yet (list-id=\(listID))")
            case .apply(let listID, let opsFile):
                print("apply not implemented yet (list-id=\(listID), ops-file=\(opsFile))")
            }
        } catch {
            fputs("\(error)\n", stderr)
            exit(2)
        }
    }
}
