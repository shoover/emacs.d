import Foundation

public enum CLICommand: Equatable, Sendable {
    case list(listID: String)
    case apply(listID: String, opsFile: String)
    case ensureList(title: String)
}

public enum CommandParseError: Error, Equatable, CustomStringConvertible {
    case missingCommand
    case unknownCommand(String)
    case unknownOption(String)
    case missingValue(String)
    case missingRequiredOption(String)

    public var description: String {
        switch self {
        case .missingCommand:
            return "Missing command. Expected `list`, `apply`, or `ensure-list`."
        case .unknownCommand(let value):
            return "Unknown command: \(value)"
        case .unknownOption(let value):
            return "Unknown option: \(value)"
        case .missingValue(let option):
            return "Missing value for option: \(option)"
        case .missingRequiredOption(let option):
            return "Missing required option: \(option)"
        }
    }
}

public enum CommandParser {
    public static func parse(_ args: [String]) throws -> CLICommand {
        guard let command = args.first else {
            throw CommandParseError.missingCommand
        }

        switch command {
        case "list":
            let parsed = try parseOptions(Array(args.dropFirst()), allowedOptions: ["--list-id"])
            guard let listID = parsed["--list-id"] else {
                throw CommandParseError.missingRequiredOption("--list-id")
            }
            return .list(listID: listID)
        case "apply":
            let parsed = try parseOptions(Array(args.dropFirst()), allowedOptions: ["--list-id", "--ops-file"])
            guard let listID = parsed["--list-id"] else {
                throw CommandParseError.missingRequiredOption("--list-id")
            }
            guard let opsFile = parsed["--ops-file"] else {
                throw CommandParseError.missingRequiredOption("--ops-file")
            }
            return .apply(listID: listID, opsFile: opsFile)
        case "ensure-list":
            let parsed = try parseOptions(Array(args.dropFirst()), allowedOptions: ["--title"])
            guard let title = parsed["--title"] else {
                throw CommandParseError.missingRequiredOption("--title")
            }
            return .ensureList(title: title)
        default:
            throw CommandParseError.unknownCommand(command)
        }
    }

    private static func parseOptions(
        _ args: [String],
        allowedOptions: Set<String>
    ) throws -> [String: String] {
        var values: [String: String] = [:]
        var index = 0
        while index < args.count {
            let key = args[index]
            guard allowedOptions.contains(key) else {
                throw CommandParseError.unknownOption(key)
            }
            let valueIndex = index + 1
            guard valueIndex < args.count else {
                throw CommandParseError.missingValue(key)
            }
            values[key] = args[valueIndex]
            index += 2
        }
        return values
    }
}
