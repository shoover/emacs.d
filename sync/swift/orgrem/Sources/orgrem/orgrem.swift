import Foundation
import OrgRemCore

@main
struct OrgRemCLI {
    static func main() {
        do {
            let output = try Runner.run(
                args: Array(CommandLine.arguments.dropFirst()),
                store: EventKitReminderStore()
            )
            print(output)
        } catch {
            fputs("\(error)\n", stderr)
            exit(2)
        }
    }
}
