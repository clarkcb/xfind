import XCTest

#if !canImport(ObjectiveC)
    public func allTests() -> [XCTestCaseEntry] {
        [
            // testCase(swiftfindTests.allTests),
            testCase(FileTypesTests.allTests),
            testCase(FileUtilTests.allTests),
            testCase(FinderTests.allTests),
            testCase(FindFileTests.allTests),
            testCase(FindOptionsTests.allTests),
            testCase(FindSettingsTests.allTests),
        ]
    }
#endif
