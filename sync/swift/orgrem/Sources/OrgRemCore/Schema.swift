import Foundation

public struct ReminderDateComponents: Codable, Equatable, Sendable {
    public let year: Int?
    public let month: Int?
    public let day: Int?
    public let hour: Int?
    public let minute: Int?
    public let timeZone: String?

    enum CodingKeys: String, CodingKey {
        case year
        case month
        case day
        case hour
        case minute
        case timeZone = "time_zone"
    }

    public init(
        year: Int?,
        month: Int?,
        day: Int?,
        hour: Int?,
        minute: Int?,
        timeZone: String?
    ) {
        self.year = year
        self.month = month
        self.day = day
        self.hour = hour
        self.minute = minute
        self.timeZone = timeZone
    }
}

public struct ReminderListRecord: Codable, Equatable, Sendable {
    public let id: String
    public let title: String

    public init(id: String, title: String) {
        self.id = id
        self.title = title
    }
}

public struct ReminderRecord: Codable, Equatable, Sendable {
    public let externalID: String
    public let localID: String
    public let title: String
    public let notes: String?
    public let completed: Bool
    public let completionDate: String?
    public let start: ReminderDateComponents?
    public let due: ReminderDateComponents?
    public let url: String?
    public let lastModified: String

    enum CodingKeys: String, CodingKey {
        case externalID = "external_id"
        case localID = "local_id"
        case title
        case notes
        case completed
        case completionDate = "completion_date"
        case start
        case due
        case url
        case lastModified = "last_modified"
    }

    public init(
        externalID: String,
        localID: String,
        title: String,
        notes: String?,
        completed: Bool,
        completionDate: String?,
        start: ReminderDateComponents?,
        due: ReminderDateComponents?,
        url: String?,
        lastModified: String
    ) {
        self.externalID = externalID
        self.localID = localID
        self.title = title
        self.notes = notes
        self.completed = completed
        self.completionDate = completionDate
        self.start = start
        self.due = due
        self.url = url
        self.lastModified = lastModified
    }
}

public struct ListResponse: Codable, Equatable, Sendable {
    public let schemaVersion: Int
    public let generatedAt: String
    public let list: ReminderListRecord
    public let items: [ReminderRecord]

    enum CodingKeys: String, CodingKey {
        case schemaVersion = "schema_version"
        case generatedAt = "generated_at"
        case list
        case items
    }

    public init(schemaVersion: Int, generatedAt: String, list: ReminderListRecord, items: [ReminderRecord]) {
        self.schemaVersion = schemaVersion
        self.generatedAt = generatedAt
        self.list = list
        self.items = items
    }
}

public enum ApplyOpKind: String, Codable, Sendable {
    case create
    case update
    case delete
}

public struct ApplyOpFields: Codable, Equatable, Sendable {
    public let title: String?
    public let notes: String?
    public let completed: Bool?
    public let start: ReminderDateComponents?
    public let due: ReminderDateComponents?
    public let url: String?

    public init(
        title: String?,
        notes: String?,
        completed: Bool?,
        start: ReminderDateComponents?,
        due: ReminderDateComponents?,
        url: String?
    ) {
        self.title = title
        self.notes = notes
        self.completed = completed
        self.start = start
        self.due = due
        self.url = url
    }
}

public struct ApplyOp: Codable, Equatable, Sendable {
    public let op: ApplyOpKind
    public let clientRef: String?
    public let externalID: String?
    public let ifLastModified: String?
    public let fields: ApplyOpFields?

    enum CodingKeys: String, CodingKey {
        case op
        case clientRef = "client_ref"
        case externalID = "external_id"
        case ifLastModified = "if_last_modified"
        case fields
    }

    public init(
        op: ApplyOpKind,
        clientRef: String?,
        externalID: String?,
        ifLastModified: String?,
        fields: ApplyOpFields?
    ) {
        self.op = op
        self.clientRef = clientRef
        self.externalID = externalID
        self.ifLastModified = ifLastModified
        self.fields = fields
    }
}

public struct ApplyRequest: Codable, Equatable, Sendable {
    public let schemaVersion: Int
    public let ops: [ApplyOp]

    enum CodingKeys: String, CodingKey {
        case schemaVersion = "schema_version"
        case ops
    }

    public init(schemaVersion: Int, ops: [ApplyOp]) {
        self.schemaVersion = schemaVersion
        self.ops = ops
    }
}

public enum ApplyStatus: String, Codable, Equatable, Sendable {
    case ok
    case notFound = "not_found"
    case conflict
    case error
}

public struct ApplyResultItem: Codable, Equatable, Sendable {
    public let externalID: String
    public let localID: String
    public let lastModified: String

    enum CodingKeys: String, CodingKey {
        case externalID = "external_id"
        case localID = "local_id"
        case lastModified = "last_modified"
    }

    public init(externalID: String, localID: String, lastModified: String) {
        self.externalID = externalID
        self.localID = localID
        self.lastModified = lastModified
    }
}

public struct ApplyResultRecord: Codable, Equatable, Sendable {
    public let opIndex: Int
    public let status: ApplyStatus
    public let clientRef: String?
    public let item: ApplyResultItem?
    public let code: String?
    public let message: String?

    enum CodingKeys: String, CodingKey {
        case opIndex = "op_index"
        case status
        case clientRef = "client_ref"
        case item
        case code
        case message
    }

    public init(
        opIndex: Int,
        status: ApplyStatus,
        clientRef: String?,
        item: ApplyResultItem?,
        code: String?,
        message: String?
    ) {
        self.opIndex = opIndex
        self.status = status
        self.clientRef = clientRef
        self.item = item
        self.code = code
        self.message = message
    }
}

public struct ApplyResponse: Codable, Equatable, Sendable {
    public let schemaVersion: Int
    public let appliedAt: String
    public let results: [ApplyResultRecord]

    enum CodingKeys: String, CodingKey {
        case schemaVersion = "schema_version"
        case appliedAt = "applied_at"
        case results
    }

    public init(schemaVersion: Int, appliedAt: String, results: [ApplyResultRecord]) {
        self.schemaVersion = schemaVersion
        self.appliedAt = appliedAt
        self.results = results
    }
}
