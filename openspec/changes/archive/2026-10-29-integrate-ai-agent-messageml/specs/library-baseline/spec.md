# Spec Delta

## Purpose

Records which Java version and Markdown parser version the library builds and runs on, and states that existing MessageML, PresentationML, Markdown and text output stays the same across a baseline upgrade.

## ADDED Requirements

### Requirement: Java 17 runtime baseline
The library SHALL compile to Java 17 bytecode and SHALL require a Java 17 or newer runtime. Continuous integration, release and CVE-scanning builds SHALL run on JDK 17.

#### Scenario: Class file version
- **WHEN** the released jar is inspected
- **THEN** its classes target Java 17 (class file major version 61)

### Requirement: Single commonmark version
The library SHALL depend on one commonmark version, at least 0.23 (the first version with the inline content parser API), and on the matching GFM tables extension. The library SHALL use this version both for the existing Markdown parser and for the extended Markdown renderer.

#### Scenario: Dependency tree
- **WHEN** the dependency tree of the library is listed
- **THEN** `org.commonmark:commonmark` and `org.commonmark:commonmark-ext-gfm-tables` appear once, at the same version

### Requirement: Existing behavior unchanged by the upgrade
For any input the existing test suite covers, `MessageMLContext` SHALL produce the same MessageML tree, PresentationML, entity JSON, Markdown and plain text before and after the baseline upgrade. This includes Markdown input parsed by the existing Markdown parser.

#### Scenario: Existing suite
- **WHEN** the test suite that existed before this change is run after the upgrade
- **THEN** it passes without changes to its expected values
