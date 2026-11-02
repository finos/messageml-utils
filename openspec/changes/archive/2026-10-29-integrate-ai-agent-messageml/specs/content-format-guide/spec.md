# Spec Delta

## Purpose

Provides the written description of the extended Markdown dialect that the library renders: a compact contract block listing every special form with right and wrong examples, and a longer authoring guide for native `<messageML>` blocks. Any producer, human or program, can embed the contract in its instructions or documentation and serve the guide on demand, and the library guarantees that what the text teaches is what the renderer accepts.

## ADDED Requirements

### Requirement: Format contract block
The library SHALL expose the format contract as a constant string, in a base form and in an extended form that appends the native-block section. The base form SHALL teach the entity forms (`@[Name](mailto:...)`, `@[Name](sym:user/<id>)`, `#[tag]`, `$[TICKER]`), their wrong-use counterparts, the `:::card` and `:::expand` directives, and the Symphony room, chat and message link forms. The extended form SHALL additionally introduce the `<messageML>` block form and refer the reader to the content format guide without naming any tool, product or platform. The base form SHALL be byte-identical whether requested with or without the native-block flag set to false.

#### Scenario: Base block content
- **WHEN** the base contract block is requested
- **THEN** it starts with `[OUTPUT FORMAT]` and contains each entity form, both directive names and the three link forms

#### Scenario: Extended block
- **WHEN** the contract block is requested with native blocks enabled
- **THEN** it starts with the base block and continues with a section that shows the `<messageML>` form and tells the reader to read the content format guide first

### Requirement: Contract embedding invariants
The base contract block SHALL contain no `#{` sequence, so that it survives expression-template engines that evaluate `#{...}`. It SHALL contain no `%` character, so that it survives `String.format`. It SHALL never name MessageML, HTML or XML in any letter case, so that a reader is taught Markdown and nothing else. The extended form MAY name MessageML, because its purpose is to teach that one dialect.

#### Scenario: No expression delimiter
- **WHEN** the base block is scanned for `#{`
- **THEN** none is found

#### Scenario: Base block never names the output dialect
- **WHEN** the base block is lower-cased and scanned for `messageml`, `html` and `xml`
- **THEN** none is found

### Requirement: Content format guide text
The library SHALL expose the authoring guide for native blocks as a string loaded once from a classpath resource under the library's own package path. The guide SHALL state when to use a block, the block rules, what is not allowed (forms and their controls, templating syntax, entity references) and the supported elements, each illustrated by a `<messageML>` example. A missing resource SHALL fail fast with an `IllegalStateException`; a read failure SHALL surface as an unchecked I/O exception.

#### Scenario: Guide loads
- **WHEN** the guide text is requested
- **THEN** a non-empty string is returned that contains at least one `<messageML>` example

#### Scenario: Missing resource
- **WHEN** a resource path that does not exist is loaded
- **THEN** an `IllegalStateException` is thrown

### Requirement: Guide examples render
Every `<messageML>` example in the guide SHALL be closed and SHALL render through the native block pipeline with full render options without a `DEGRADED` outcome. This is checked by a test that parses the guide text with the library's own Markdown extension and renders each block it finds.

#### Scenario: Example corpus
- **WHEN** the guide is parsed and each native block is rendered as a final render
- **THEN** every block is closed and its outcome is `RENDERED` or `REPAIRED`
