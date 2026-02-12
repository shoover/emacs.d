// swift-tools-version: 6.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "orgrem",
    products: [
        .library(name: "OrgRemCore", targets: ["OrgRemCore"]),
        .executable(name: "orgrem", targets: ["orgrem"]),
    ],
    targets: [
        .target(
            name: "OrgRemCore"
        ),
        .executableTarget(
            name: "orgrem",
            dependencies: ["OrgRemCore"]
        ),
        .testTarget(
            name: "OrgRemCoreTests",
            dependencies: ["OrgRemCore"]
        ),
        .testTarget(
            name: "OrgRemIntegrationTests",
            dependencies: ["OrgRemCore"]
        ),
    ]
)
