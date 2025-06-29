# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.3] - 2025-06-28

### Added

- Support non-overlapping blanket impls

## [1.0.2] - 2025-06-25

### Fixed

- Correctly serialize TraitBound to tokens

## [1.0.1] - 2025-06-25

### Fixed

- Support single blanket impl

## [1.0.0] - 2025-03-24

### Added

- Stabilized API


## [0.8.0] - 2024-09-18

### Added

- Implement Substitute for every type in syn
- Implement Superset for every type in syn
- Add support for overlapping impl groups
- Add support for dispatch on generic type
- Add support for complex generic argument types

### Changed

- transition to using IndexMap instead of FxHashMap

### Fixed

- Add ?Sized bound on main trait params

## [0.7.1] - 2024-05-20

### Fixed

- Add ?Sized bound to main trait type parameters

## [0.7.0] - 2024-01-23

### Added

- Predictable ordering of generic arguments

### Changed

- Only dispatch on assoc bounds that are present in all impls

### Fixed

- Fix inherent impls parametrization
- Fix for const parameters in inherent impls

## [0.6.0] - 2024-01-08

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
