# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.5.0] - 2024-01-08

### Added

- Support bounds for main trait parameters
- Support const parameters in main trait
- Support default generic types in main trait
- Support multiple blanket impls on different trait generics
- Support for resolution of parameters not bounded by trait or self type

### Changed

- Keep original trait param identifiers

### Fixed

- Fix generation of main trait generics

## [0.5.0] - 2023-11-23

### Added

- Support multiple blanket impls on a different self type

## [0.4.0] - 2023-11-20

### Added

- Add support for lifetimes in trait definition

## [0.3.0] - 2023-10-08

### Added

- Add input validation
- Add support for dispatch on unsized types

## [0.2.0] - 2023-09-24

### Added

- Enable dispatch on where clause bounded type

### Changed

- Don't require collection of type param identifiers to be ordered

## [0.1.0] - 2023-09-23
