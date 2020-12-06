# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]

## [0.0.3] - 2020-12-06
### Added
- Added documentation to all parts of the Ansi package except IO package.
- Added Signal and Input handling.
- Finished implementing surfaces, colours and styles.
- Added support for the non-ansi-compliant windows console CMD, to support older versions of Windows.
- Finished some things marked to do.

### Fixed
- Fixed the format of this changelog.
- Fixed Put(3) procedure in Ansi.Surfaces and added style printing too.
- Fixed native support for older Windows consoles.
- Fixed some bugs and errors found while coding.

### Removed
- Removed the inlining of many of the functions and procedures throughout the package.
- Removed Ansi.Pools, it will be available in a future.


## [0.0.2] - 2020-11-21
### Added
- Surfaces have been optimized.


## [0.0.1] - 2020-11-06
### Added
- Finished Styles and Colours
- Added some tests
- Fixed some major bugs
- Completed Surfaces (only some features have to be implemented)
- Optimized update time
- Added title screen


## [0.0.0] - 2020-10-29
### Added
- Created CHANGELOG.md file
- Added GPLv3 licence read it [here](LICENCE)
- Added main file of the project [main.adb](src/main.adb)
- Added Surfaces
- Added Colouring for Surfaces
- Added Cursors
- Fixed major bugs
