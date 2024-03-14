# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

...

### Fixed

...

### Changed

...

## [0.3.0.0] - 2024-03-13

### Added

  * Configurable maximum post length
  * Configuration for client sent an empty query language
  * Config options for spacecookie server
  * (Hopefully) more accessible reply + new thread links

### Fixed

  * Give an error for when a post is too long

## [0.2.0.0] - 2024-03-08

### Added

  * Rate-limiting, now you can only create a thread once per n minutes and create a reply once per x minutes
  * TOML config!

### Changed

  * Timestamps in the database stored with timezone information now

## [0.1.0.0] - 2024-03-04

First release, a proof of concept.

[unreleased]: https://github.com/someodd/gopherden/compare/v0.3.0.0...HEAD
[0.3.0.0]: https://github.com/someodd/gopherden/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/someodd/gopherden/compare/v0.1.0.0...v0.2.0.0
[0.1.0.0]: https://github.com/someodd/gopherden/release/v0.1.0.0
