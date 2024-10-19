//
//  FindError.swift
//

import Foundation

final public class FindError: Error {
    public let msg: String

    public init(msg: String) {
        self.msg = msg
    }
}
