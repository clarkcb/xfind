//
//  common.swift
//  swiftfind
//
//  Created by Cary Clark on 5/20/15.
//  Copyright (c) 2015 Cary Clark. All rights reserved.
//

import Foundation

public let ISO_DATE_FORMAT = "yyyy-MM-dd"

public let whitespace = CharacterSet(charactersIn: " \t\r\n")

public func logMsg(_ str: String) {
    print(str)
}

public func logError(_ str: String) {
    logMsg("ERROR: \(str)")
}

// from http://ijoshsmith.com/2014/06/18/create-a-swift-dictionary-from-an-array/
func toDictionary<E, K, V>(
    _ array: [E],
    transformer: (_ element: E) -> (key: K, value: V)?
) -> [K: V] {
    array.reduce([:]) { dict, e in
        var d = dict
        if let (key, value) = transformer(e) {
            d[key] = value
        }
        return d
    }
}

public func arrayToString(_ arr: [String]) -> String {
    var str: String = "["
    var count: Int = 0
    for elem in arr {
        if count > 0 {
            str += ", "
        }
        str += "\"\(elem)\""
        count += 1
    }
    str += "]"
    return str
}

public func arrayToString(_ arr: [Regex]) -> String {
    arrayToString(arr.map(\.pattern))
}

public func setToString(_ set: Set<String>) -> String {
    arrayToString(Array(set.sorted()))
}

public func dateToString(_ date: Date?) -> String {
    if date == nil {
        return "0"
    } else {
        return date!.description
    }
}

public func stringToDate(_ s: String) -> Date? {
    let formatter: DateFormatter = DateFormatter()
    formatter.dateFormat = ISO_DATE_FORMAT
    return formatter.date(from: s)
}

public func take<T>(_ seq: [T], num: Int) -> [T] {
    var newSeq: [T] = []
    var taken = 0
    while taken < num, taken < seq.count {
        newSeq.append(seq[taken])
        taken += 1
    }
    return newSeq
}

public func takeRight<T>(_ seq: [T], num: Int) -> [T] {
    var right: [T] = []
    var sub = 1
    while sub <= num, sub <= seq.count {
        right.append(seq[seq.count - sub])
        sub += 1
    }
    return Array(right.reversed())
}

// for printing the borders in multiline find results
extension String {
    public func `repeat`(_ n: Int) -> String {
        var result = self
        for _ in 1 ..< n {
            result += self
        }
        return result
    }
}

// https://www.avanderlee.com/swift/unique-values-removing-duplicates-array/
public extension Sequence where Iterator.Element: Hashable {
    func unique() -> [Iterator.Element] {
        var seen: Set<Iterator.Element> = []
        return filter { seen.insert($0).inserted }
    }
}

// let array: [Int] = [1, 1, 1, 2, 2, 2, 3, 3, 3]
// print(array.unique()) // prints: [1, 2, 3]
