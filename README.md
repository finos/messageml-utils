[![FINOS - Graduated](https://cdn.jsdelivr.net/gh/finos/contrib-toolbox@master/images/badge-graduated.svg)](https://community.finos.org/docs/governance/lifecycle-stages/graduated)
[![OpenSSF Best Practices](https://bestpractices.coreinfrastructure.org/projects/6756/badge)](https://bestpractices.coreinfrastructure.org/projects/6756)
[![Licence](https://img.shields.io/badge/licence-Apache%20Licence%20%282.0%29-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
[![Maven Central](https://img.shields.io/maven-central/v/org.symphonyoss.symphony/messageml.svg)](http://search.maven.org/#search%7Cga%7C1%7Ca%3A%22messageml%22)
[![Build Status](https://github.com/finos/messageml-utils/workflows/Build/badge.svg)](https://github.com/finos/messageml-utils/actions)
[![Maintainability Rating](https://sonarcloud.io/api/project_badges/measure?project=symphonyoss_messageml-utils&metric=sqale_rating)](https://sonarcloud.io/dashboard?id=symphonyoss_messageml-utils)
[![Sonar](https://sonarcloud.io/api/project_badges/measure?project=symphonyoss_messageml-utils&metric=coverage)](https://sonarcloud.io/dashboard?id=symphonyoss_messageml-utils)

# Introduction

MessageML is a markup language used by the Symphony Agent API for representing messages, including formatting (bold,
italic, numbered and unnumbered lists etc.) and entity data representing [_structured
objects_](https://docs.developers.symphony.com/building-bots-on-symphony/messages/overview-of-messageml/entities/structured-objects).

The format is intended to allow third parties to create and render rich content messages representing complex objects,
enabling deep workflow integrations across multiple systems, with Symphony being the central hub.

# Description

Support of Symphony structured objects comprises the ability to parse and render the following formats:

* [MessageML](https://docs.developers.symphony.com/building-bots-on-symphony/messages/overview-of-messageml), a superset of PresentationML, adding a number of
  convenience tags for more complex or frequently used constructs. It is translated to PresentationML internally by the
  Agent API before ingestion in Symphony.
* [PresentationML](https://docs.developers.symphony.com/building-bots-on-symphony/messages/overview-of-presentationml), a strict subset of HTML5
  describing the markup which controls the formatting (presentation) of a message. PresentationML is the format used
  internally by Symphony and returned from message ingestion and retrieval by the Agent API.
* [EntityJSON](https://docs.developers.symphony.com/building-bots-on-symphony/messages/overview-of-messageml/entities), JSON data representing
  complex financial objects and other structured data. EntityJSON elements are referenced in PresentationML and are
  provided by the message originator or generated automatically during the expansion of MessageML convenience tags.
* [Freemarker](http://freemarker.org/), a templating language providing macros (conditional blocks, iterations,
  assignments etc.)
  to expand message templates into MessageML messages. Freemarker support is experimental.
* [Markdown](http://daringfireball.net/projects/markdown/syntax), the legacy text format used by Symphony. Markdown
  parsing and rendering capabilities are provided for compatibility with older versions of Symphony.

More specifically, the MessageML parser can be used to:

* validate the input message as correct MessageML or PresentationML and the input data as correct EntityJSON; if the
  input message contains references to entity data, the validation process verifies if the provided data matches those
  references
* expand Freemarker templates into MessageML messages; this includes processing Freemarker macros and injecting
  EntityJSON data referenced by template variables
* construct a document tree representing the message
* serialize the MessageML tree as PresentationML; this step includes generation of EntityJSON data from convenience
  MessageML tags
* serialize the MessageML tree as Markdown
* parse Markdown input into a MessageML document tree

# Usage

```java
/* Instantiate the parsing context. The "dataProvider" object is used to resolve user mentions and check supplied URLs against a whitelist of supported URI schemes. */
MessageMLContext context = new MessageMLContext(/*IDataProvider*/ dataProvider);

/* Parse the message and entity data */
context.parseMessageML(/*String*/ message, /*String*/ entityJSON, /*String*/ version);

/* Parse a Markdown message into a MessageML document tree */
context.parseMarkdown(/*String*/ markdown, /*JsonNode*/ entities, /*JsonNode*/ media);

/* Get the MessageML document tree */
Element messageML = context.getMessageML();

/* Get the PresenttionML representation of the message */
String presentationML = context.getPresentationML();

/* Get the entity data of the message, including data auto-generated from MessageML convenience tags*/
String entityJSON = context.getEntityJson();

/* Get the Markdown representation of the message */
String markdown = context.getMarkdown();

/* Get the plain text of the message */
String text = context.getText();
```

## Requirements

The library requires Java 17 or newer (since version 2.0.0). Projects still on Java 8 or 11 can stay on the 1.x line.

# Composing MessageML from extended Markdown

The `org.finos.symphony.messageml.messagemlutils.compose` package turns authored input into MessageML that the
library accepts, and makes MessageML fragments safe to send. It is independent of `MessageMLContext` and of the
legacy Markdown support above.

## Rendering extended Markdown

`MarkdownToMessageMLConverter` converts Markdown (CommonMark plus GFM tables) into a MessageML body or document. On top
of standard Markdown it understands an extended dialect:

* Symphony entities: `@[Jane Doe](mailto:jane.doe@example.com)` or `@[Jane Doe](sym:user/12345)` for mentions,
  `#[quarterly-review]` for hashtags and `$[AAPL]` for cashtags
* container directives: `:::card` (a card with a header and a body) and `:::expand Title` (an expandable card)
* native `<messageML>` blocks, which are inlined after going through the repair pipeline below
* Symphony room, chat and message links, which are normalized to their canonical form

The converter is stateless and thread-safe. Each call takes an immutable `MessageMLRenderOptions` value:
`MessageMLRenderOptions.LEGACY` disables the extended dialect, `MessageMLRenderOptions.full()` enables entity syntax and
container directives, and `withNativeMessageML(true)` additionally enables native blocks.

```java
MarkdownToMessageMLConverter converter = new MarkdownToMessageMLConverter();
MessageMLRenderOptions options = MessageMLRenderOptions.full().withNativeMessageML(true);

String markdown = "Filed under #[quarterly-review] for @[Jane Doe](mailto:jane.doe@example.com).\n"
    + "\n"
    + ":::card\n"
    + "### Summary\n"
    + "All checks passed.\n"
    + ":::\n";

/* A complete <messageML> document */
String messageML = converter.renderDocument(markdown, options);

/* The body only, with counts of the entity forms, cards and native blocks that were produced */
RenderedMessageML rendered = converter.render(markdown, options);
String body = rendered.body();
FormCounts forms = rendered.forms();

/* An intermediate update of a message that is still being written: expandable cards are shown expanded and
   a native block that cannot render yet is replaced by a placeholder */
String partial = converter.renderDocument(markdown, options.asIntermediate());
```

`SymphonySyntaxNormalizer.toPlainMarkdown(String)` reduces the extended dialect back to plain Markdown, and
`StoredMessageText.separateGroupedContent(String)` keeps card headers and bodies apart when the text of a stored
PresentationML message is extracted. `SymphonyLinks` builds and normalizes Symphony deep links.

## Validating and repairing MessageML

`MessageMLValidator` checks a document with the library's own parser and offers deterministic, error-directed repair
and an escaped plain-text fallback:

```java
MessageMLValidator validator = new MessageMLValidator();

String error = validator.validate(messageML, entityJson);             // null when valid
if (error != null) {
  String repaired = validator.repair(messageML, entityJson, error);  // null when no rule applies
  if (repaired == null || validator.validate(repaired, entityJson) != null) {
    messageML = validator.escapedFallback(messageML, plainText);     // always valid
  } else {
    messageML = repaired;
  }
}
```

The fragment pipeline is also available step by step: `MessageMLSanitizer` rejects templating directives, Symphony
Elements forms and entity references, and escapes interpolations; `MessageMLBalancer` closes a fragment that was cut
short; `MessageMLStructureRepairer` rebuilds well-formed markup from tag soup. `NativeMessageMLRenderer` chains them
with validation and repair for one native block and reports whether the block was `RENDERED`, `REPAIRED` or
`DEGRADED` (replaced by a collapsed card that shows the original source).

## The dialect's reference text

The `compose.format` package holds the written description of the extended dialect, kept next to the code that
implements it. `OutputFormatContract.block(boolean)` returns a compact contract that lists every special form with
right and wrong examples, suitable for embedding in documentation or in instructions given to an author.
`SymphonyContentFormatGuide.text()` returns the longer authoring guide for native `<messageML>` blocks. Every
`<messageML>` example in the guide is rendered by the test suite, so the guide only teaches what the renderer accepts.

## Contributing

1. Fork it (<https://github.com/finos/messageml-utils/fork>)
2. Create your feature branch (`git checkout -b feature/fooBar`)
3. Read our [contribution guidelines](.github/CONTRIBUTING.md)
   and [Community Code of Conduct](https://www.finos.org/code-of-conduct)
4. Commit your changes (`git commit -am 'Add some fooBar'`)
5. Push to the branch (`git push origin feature/fooBar`)
6. Create a new Pull Request

_NOTE:_ Commits and pull requests to FINOS repositories will only be accepted from those contributors with an active,
executed Individual Contributor License Agreement (ICLA) with FINOS OR who are covered under an existing and active
Corporate Contribution License Agreement (CCLA) executed with FINOS. Commits from individuals not covered under an ICLA
or CCLA will be flagged and blocked by the FINOS Clabot tool. Please note that some CCLAs require individuals/employees
to be explicitly named on the CCLA.

*Need an ICLA? Unsure if you are covered under an existing CCLA? Email [help@finos.org](mailto:help@finos.org)*

## License

The code in this repository is distributed under
the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).

Copyright 2016-2021 Symphony LLC