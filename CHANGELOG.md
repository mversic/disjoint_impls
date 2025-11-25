# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.3.1] - 2025-11-25
### Fixed
- forbid non-qualified associated types

## [1.3.0] - 2025-11-24
### Added
- support constraints in generic arguments
- dispatch overlapping nested subgroups

### Fixed
- use fundametal types to determine type disjointness
- generalize elided lifetimes on function arguments

## [1.2.3] - 2025-11-05
### Fixed
- dispatch on all possible trait combinations
- separate disjoint impls eagerly

## [1.2.2] - 2025-11-04
### Fixed
- generalize impl signatures, don't look for supersets

## [1.2.1] - 2025-10-17
### Added
- support GATs

## [1.2.0] - 2025-10-13
### Fixed
- require that at least one solution always exists even if overlapping
- on intersection check if impl has generalization with the group

## [1.1.3] - 2025-10-07
### Fixed
- silence `clippy::needless_lifetimes` lint

## [1.1.2] - 2025-10-07
### Changed
- don't use unconstrained type parameters in associated bindings

## [1.1.1] - 2025-10-06
### Fixed
- make all impls present in the output

## [1.1.0] - 2025-10-05

### Fixed
- handle overlapping payloads when forming an impl group

### Changed
- new impl resolution mechanism

### Removed
- disallow unspecified default trait arguments

## [1.0.9] - 2025-09-09

### Added
- support for foreign/remote traits
- support for unsafe methods and traits

### Fixed
- correctly name elided lifetimes

### Changed
- move ambiguous associated param check to validation step

## [1.0.8] - 2025-08-30

### Added
- handle elided lifetimes
- enable dispatching on associated types

### Fixed
- allow overlapping assoc bindings
- compare item ids when checking for assoc bindings overlap

### Changed
- simplify implementation for finding overlapping impls
- simplify impl group id topsort implementation

## [1.0.7] - 2025-08-24

### Added
- support dispatching on associated type

### Fixed
- properly rename generic arguments in impl trait path

## [1.0.6] - 2025-08-24

### Fixed
- properly define main trait generics
- don't consider associated bounds on concrete types

### Changed
- simplify tracking of associated binding groups
- implementation of wrapper types (most notably of `TraitBound`)
- rename assoc bound to assoc binding
- optimize algorithm for finding impl groups

## [1.0.5] - 2025-08-12

### Added
- Add support for associated type bounds

## [1.0.4] - 2025-08-11

### Added
- Add compile tests
- Implement support for params only constrained by other params

### Fixed
- Properly resolve main trait bounded default params
- Add param bounds for complex predicates to main trait impl
- Properly index parameters

### Changed
- Let single impls pass through without a helper trait

### Removed
- Remove main trait assoc bounds from assoc bounds group for dispatch

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

### Fixed
- Add ?Sized bound on main trait params

### Changed
- transition to using IndexMap instead of FxHashMap

## [0.7.1] - 2024-05-20

### Fixed
- Add ?Sized bound to main trait type parameters

## [0.7.0] - 2024-01-23

### Added
- Predictable ordering of generic arguments

### Fixed
- Fix inherent impls parametrization
- Fix for const parameters in inherent impls

### Changed
- Only dispatch on assoc bounds that are present in all impls

## [0.6.0] - 2024-01-08

### Added
- Support bounds for main trait parameters
- Support const parameters in main trait
- Support default generic types in main trait
- Support multiple blanket impls on different trait generics
- Support for resolution of parameters not bounded by trait or self type

### Fixed
- Fix generation of main trait generics

### Changed
- Keep original trait param identifiers

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
