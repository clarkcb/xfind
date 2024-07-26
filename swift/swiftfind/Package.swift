// swift-tools-version:5.10
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "swiftfind",
    // products already default to swiftfind.framework, swiftfindApp and swfitfindTests.xctest
    products: [
        .library(name: "swiftfind", targets: ["swiftfind"]),
        .executable(name: "swiftfindApp", targets: ["swiftfindApp"])
    ],
    dependencies: [
        // Despite the warning about being deprecated, name must be included here
        // because there's a discrepancy between its package name and target name
        .package(name: "SQLite", url: "https://github.com/stephencelis/SQLite.swift.git", .upToNextMajor(from: "0.15.3")),
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages this package depends on.
        .target(
            name: "swiftfind",
            dependencies: ["SQLite"]),
        .executableTarget(
            name: "swiftfindApp",
            dependencies: ["swiftfind"]),
        .testTarget(
            name: "swiftfindTests",
            dependencies: ["swiftfind"])
    ]
)
