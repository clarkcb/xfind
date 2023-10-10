//
//  FindError.swift
//

import Foundation

public class FindError: Error {
    public let msg: String

    public init(msg: String) {
        self.msg = msg
    }
}
