// swift-tools-version: 5.10
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "objcfind",
    products: [
        // Products define the executables and libraries a package produces, and make them visible to other packages.
        .library(
            name: "objcfind",
            targets: ["objcfind"]),
        .executable(
            name: "objcfindApp",
            targets: ["objcfindApp"])
    ],
    dependencies: [
        // Dependencies declare other packages that this package depends on.
        // .package(url: /* package url */, from: "1.0.0"),
//        .package(
//            name: "magic",
//            path: "/usr/local/lib/libmagic.dylib"
//        )
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages this package depends on.
//        .systemLibrary(
//            name: "libmagic",
//            providers: [
//                .brew(["libmagic"])
//            ]
//        ),
        .target(
            name: "objcfind",
            dependencies: [
//                .target(name: "libmagic")
            ]),
        .executableTarget(
            name: "objcfindApp",
            dependencies: ["objcfind"]),
        .testTarget(
            name: "objcfindTests",
            dependencies: ["objcfind"]),
    ]
)
