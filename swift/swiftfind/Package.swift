// swift-tools-version:5.3
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
        // Dependencies declare other packages that this package depends on.
        // .package(url: /* package url */, from: "1.0.0"),
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages this package depends on.
        .target(
            name: "swiftfind",
            dependencies: []),
        .target(
            name: "swiftfindApp",
            dependencies: ["swiftfind"]),
        .testTarget(
            name: "swiftfindTests",
            dependencies: ["swiftfind"])
    ]
)
