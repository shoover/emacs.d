import EventKit
import Foundation
import OrgRemCore

enum EventKitStoreError: Error, CustomStringConvertible {
    case accessDenied
    case listNotFound(String)
    case reminderNotFound(String)
    case noReminderSource

    var description: String {
        switch self {
        case .accessDenied:
            return "Reminders access denied."
        case .listNotFound(let id):
            return "Reminders list not found: \(id)"
        case .reminderNotFound(let externalID):
            return "Reminder not found for external_id: \(externalID)"
        case .noReminderSource:
            return "Unable to resolve a reminders source for list creation."
        }
    }
}

final class EventKitReminderStore: ReminderStore {
    private let eventStore = EKEventStore()
    private let dateFormatter = ISO8601DateFormatter()

    func list(listID: String) throws -> (ReminderListRecord, [ReminderRecord]) {
        try ensureAccess()
        let calendar = try calendar(withID: listID)
        let reminders = try fetchReminders(in: [calendar])
        let records = reminders.compactMap { reminder in
            record(from: reminder)
        }
        return (ReminderListRecord(id: calendar.calendarIdentifier, title: calendar.title), records)
    }

    func apply(listID: String, request: ApplyRequest) throws -> ApplyResponse {
        try ensureAccess()
        let calendar = try calendar(withID: listID)
        var results: [ApplyResultRecord] = []

        for (index, op) in request.ops.enumerated() {
            do {
                switch op.op {
                case .create:
                    let reminder = EKReminder(eventStore: eventStore)
                    reminder.calendar = calendar
                    applyFields(op.fields, to: reminder, isCreate: true)
                    try eventStore.save(reminder, commit: true)
                    guard let item = record(from: reminder) else {
                        results.append(.init(opIndex: index, status: .error, clientRef: op.clientRef, item: nil, code: "save_failed", message: "Saved reminder missing identifiers"))
                        continue
                    }
                    results.append(.init(
                        opIndex: index,
                        status: .ok,
                        clientRef: op.clientRef,
                        item: .init(externalID: item.externalID, localID: item.localID, lastModified: item.lastModified),
                        code: nil,
                        message: nil
                    ))
                case .update:
                    guard let externalID = op.externalID else {
                        results.append(.init(opIndex: index, status: .error, clientRef: op.clientRef, item: nil, code: "missing_external_id", message: "Update requires external_id"))
                        continue
                    }
                    guard let reminder = try reminder(byExternalID: externalID, calendarID: calendar.calendarIdentifier) else {
                        results.append(.init(opIndex: index, status: .notFound, clientRef: op.clientRef, item: nil, code: nil, message: nil))
                        continue
                    }
                    if let expected = op.ifLastModified,
                       isoString(for: reminder.lastModifiedDate) != expected {
                        results.append(.init(opIndex: index, status: .conflict, clientRef: op.clientRef, item: nil, code: nil, message: nil))
                        continue
                    }
                    applyFields(op.fields, to: reminder, isCreate: false)
                    try eventStore.save(reminder, commit: true)
                    guard let item = record(from: reminder) else {
                        results.append(.init(opIndex: index, status: .error, clientRef: op.clientRef, item: nil, code: "save_failed", message: "Saved reminder missing identifiers"))
                        continue
                    }
                    results.append(.init(
                        opIndex: index,
                        status: .ok,
                        clientRef: op.clientRef,
                        item: .init(externalID: item.externalID, localID: item.localID, lastModified: item.lastModified),
                        code: nil,
                        message: nil
                    ))
                case .delete:
                    guard let externalID = op.externalID else {
                        results.append(.init(opIndex: index, status: .error, clientRef: op.clientRef, item: nil, code: "missing_external_id", message: "Delete requires external_id"))
                        continue
                    }
                    guard let reminder = try reminder(byExternalID: externalID, calendarID: calendar.calendarIdentifier) else {
                        results.append(.init(opIndex: index, status: .notFound, clientRef: op.clientRef, item: nil, code: nil, message: nil))
                        continue
                    }
                    if let expected = op.ifLastModified,
                       isoString(for: reminder.lastModifiedDate) != expected {
                        results.append(.init(opIndex: index, status: .conflict, clientRef: op.clientRef, item: nil, code: nil, message: nil))
                        continue
                    }
                    try eventStore.remove(reminder, commit: true)
                    results.append(.init(opIndex: index, status: .ok, clientRef: op.clientRef, item: nil, code: nil, message: nil))
                }
            } catch {
                results.append(.init(opIndex: index, status: .error, clientRef: op.clientRef, item: nil, code: "exception", message: String(describing: error)))
            }
        }

        return ApplyResponse(schemaVersion: 1, appliedAt: dateFormatter.string(from: Date()), results: results)
    }

    func ensureList(title: String) throws -> ReminderListRecord {
        try ensureAccess()
        if let existing = eventStore.calendars(for: .reminder)
            .first(where: { $0.title == title }) {
            return ReminderListRecord(id: existing.calendarIdentifier, title: existing.title)
        }

        let calendar = EKCalendar(for: .reminder, eventStore: eventStore)
        let source = eventStore.defaultCalendarForNewReminders()?.source
            ?? eventStore.sources.first(where: { $0.sourceType == .local })
            ?? eventStore.sources.first
        guard let source else {
            throw EventKitStoreError.noReminderSource
        }
        calendar.source = source
        calendar.title = title
        try eventStore.saveCalendar(calendar, commit: true)
        return ReminderListRecord(id: calendar.calendarIdentifier, title: calendar.title)
    }

    private func ensureAccess() throws {
        let status = EKEventStore.authorizationStatus(for: .reminder)
        switch status {
        case .fullAccess:
            return
        case .notDetermined:
            final class AccessResult: @unchecked Sendable {
                var granted = false
                var requestError: Error?
            }

            let sem = DispatchSemaphore(value: 0)
            let result = AccessResult()
            if #available(macOS 14.0, *) {
                eventStore.requestFullAccessToReminders { ok, error in
                    result.granted = ok
                    result.requestError = error
                    sem.signal()
                }
            } else {
                eventStore.requestAccess(to: .reminder) { ok, error in
                    result.granted = ok
                    result.requestError = error
                    sem.signal()
                }
            }
            sem.wait()
            if let requestError = result.requestError {
                throw requestError
            }
            if !result.granted {
                throw EventKitStoreError.accessDenied
            }
        case .writeOnly, .denied, .restricted:
            throw EventKitStoreError.accessDenied
        @unknown default:
            throw EventKitStoreError.accessDenied
        }
    }

    private func calendar(withID id: String) throws -> EKCalendar {
        guard let calendar = eventStore.calendar(withIdentifier: id) else {
            throw EventKitStoreError.listNotFound(id)
        }
        return calendar
    }

    private func fetchReminders(in calendars: [EKCalendar]) throws -> [EKReminder] {
        let sem = DispatchSemaphore(value: 0)
        var fetched: [EKReminder] = []
        let predicate = eventStore.predicateForReminders(in: calendars)
        eventStore.fetchReminders(matching: predicate) { reminders in
            fetched = reminders ?? []
            sem.signal()
        }
        sem.wait()
        return fetched
    }

    private func reminder(byExternalID externalID: String, calendarID: String) throws -> EKReminder? {
        let items = eventStore.calendarItems(withExternalIdentifier: externalID)
        return items
            .compactMap { $0 as? EKReminder }
            .first(where: { $0.calendar.calendarIdentifier == calendarID })
    }

    private func record(from reminder: EKReminder) -> ReminderRecord? {
        guard let externalID = reminder.calendarItemExternalIdentifier else {
            return nil
        }
        return ReminderRecord(
            externalID: externalID,
            localID: reminder.calendarItemIdentifier,
            title: reminder.title ?? "",
            notes: reminder.notes,
            completed: reminder.isCompleted,
            completionDate: isoString(for: reminder.completionDate),
            start: components(from: reminder.startDateComponents),
            due: components(from: reminder.dueDateComponents),
            url: reminder.url?.absoluteString,
            lastModified: isoString(for: reminder.lastModifiedDate) ?? dateFormatter.string(from: Date())
        )
    }

    private func components(from value: DateComponents?) -> ReminderDateComponents? {
        guard let value else { return nil }
        return ReminderDateComponents(
            year: value.year,
            month: value.month,
            day: value.day,
            hour: value.hour,
            minute: value.minute,
            timeZone: value.timeZone?.identifier
        )
    }

    private func dateComponents(from value: ReminderDateComponents?) -> DateComponents? {
        guard let value else { return nil }
        var comps = DateComponents()
        comps.calendar = Calendar(identifier: .gregorian)
        comps.year = value.year
        comps.month = value.month
        comps.day = value.day
        comps.hour = value.hour
        comps.minute = value.minute
        if let timeZone = value.timeZone {
            comps.timeZone = TimeZone(identifier: timeZone)
        }
        return comps
    }

    private func applyFields(_ fields: ApplyOpFields?, to reminder: EKReminder, isCreate: Bool) {
        guard let fields else { return }
        if let title = fields.title {
            reminder.title = title
        }
        if let notes = fields.notes {
            reminder.notes = notes
        }
        if let completed = fields.completed {
            reminder.isCompleted = completed
        } else if isCreate {
            reminder.isCompleted = false
        }
        if let start = fields.start {
            reminder.startDateComponents = dateComponents(from: start)
        } else if isCreate {
            reminder.startDateComponents = nil
        }
        if let due = fields.due {
            reminder.dueDateComponents = dateComponents(from: due)
        } else if isCreate {
            reminder.dueDateComponents = nil
        }
        if let urlString = fields.url {
            if urlString.isEmpty {
                reminder.url = nil
            } else {
                reminder.url = URL(string: urlString)
            }
        } else if isCreate {
            reminder.url = nil
        }
    }

    private func isoString(for date: Date?) -> String? {
        guard let date else { return nil }
        return dateFormatter.string(from: date)
    }
}
