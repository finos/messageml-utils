# Proposal

## Why

`symphony-ai-platform/ai-agent-messageml` holds roughly 3,300 lines of general-purpose MessageML tooling: an extended Markdown-to-MessageML converter (Symphony entity syntax, `:::card` / `:::expand` directives, inline native `<messageML>` blocks), a sanitize/balance/validate/repair/degrade pipeline for MessageML fragments, and the written description of that extended Markdown dialect (a compact format contract and a longer authoring guide for native blocks). None of it depends on Spring, the BDK or the AI platform. It already depends on `messageml-utils`, and it is consumed by more than one project (`ai-agent-supervisor`, `fluxnova-symphony-bpmn`). Moving it into `messageml-utils` gives it one home next to the parser it builds on, lets any MessageML producer reuse it (not only the AI platform), keeps the dialect's documentation next to the code that implements it, and removes a private module that duplicates the library's Markdown concern.

## What Changes

- Add the `ai-agent-messageml` production code to `messageml-utils` under a new package `org.finos.symphony.messageml.messagemlutils.compose` (with `.compose.parser` for the commonmark extensions and `.compose.format` for the dialect description). The package name is deliberately generic: the code composes MessageML from authored input and makes fragments sendable, whoever the author is. This covers:
  - Markdown rendering: `MarkdownToMessageMLConverter`, `MessageMLRenderOptions`, `RenderedMessageML`, `FormCounts`, `HtmlEntities`, the parser extensions (`SymphonyMarkdownExtension`, `SymphonyEntityInlineParser`/`SymphonyEntityNode`, `ContainerDirective` + block parser, `MessageMLBlock` + block parser), `SymphonyLinks`, `SymphonySyntaxNormalizer` and `StoredMessageText`.
  - Validation and repair: `MessageMLValidator`, `MessageMLSanitizer`, `MessageMLTagScanner`, `MessageMLBalancer`, `MessageMLStructureRepairer`, `NativeMessageMLRenderer`.
  - Format description: `OutputFormatContract` (the compact description of the extended Markdown forms, suitable for embedding in a prompt or in documentation) and `SymphonyContentFormatGuide` with its `symphony-content-format-guide.md` resource (the authoring guide for native `<messageML>` blocks). The one sentence that names a platform-specific tool is generalized; the class names and method signatures are unchanged.
- Port the matching unit tests and their fixtures (`native-messageml-fixtures/`, `messageml-doc-corpus/`) so the behavior is pinned in this repo. This includes the format tests: every `<messageML>` example in the guide must render without degrading, and the contract text must keep its embedding invariants.
- Strip internal references (Jira keys, `design.md` sections of the AI platform, CAIP decision IDs, prompt-benchmark narrative, platform tool names) from Javadoc and text. Keep the explanation where it describes behavior.
- **BREAKING**: raise the library baseline from Java 1.8 to Java 17 (compiler `release` 17, CI and release workflows on JDK 17). Consumers still on Java 8 or 11 stay on the 1.x line.
- **BREAKING (transitive)**: upgrade `org.commonmark:commonmark` from 0.15.2 to 0.30.0 and add `commonmark-ext-gfm-tables` 0.30.0. Adapt the existing `markdown` package (`EntityDelimiterProcessor` and any other API break) so `MessageMLContext` output is unchanged.
- Release as a new major version (2.0.0).

Out of scope:
- Migrating `symphony-ai-platform` consumers to the new coordinates and deleting `ai-agent-messageml`. That is a follow-up once 2.0.0 is published.
- Merging the new converter with the existing `MarkdownParser`. They stay separate APIs.
- Any wording change to the format contract beyond removing the platform tool name. The text is a measured deliverable and is ported as is.

## Capabilities

### New Capabilities
- `extended-markdown-rendering`: converting extended Markdown (entity syntax, container directives, native MessageML blocks, Symphony links) into a MessageML body or document, plus the plain-text helpers for stored messages.
- `messageml-fragment-repair`: validating a MessageML document against the library, deterministic sanitize/balance/structural and error-directed repair of fragments, and the degraded fallback.
- `content-format-guide`: the library-provided description of the extended Markdown dialect: a compact contract block with embedding invariants, and an authoring guide whose examples are guaranteed to render.
- `library-baseline`: the Java and commonmark baseline the library builds and runs on, and the guarantee that existing `MessageMLContext` behavior is unchanged by the upgrade.

### Modified Capabilities
<!-- None: openspec/specs/ is empty, so there are no existing capability specs to modify. -->

## Impact

- **Code**: about 22 new production classes and one text resource under `org.finos.symphony.messageml.messagemlutils.compose`, with about 22 test classes. Small edits in `markdown/EntityDelimiterProcessor.java` (and possibly `MarkdownParser`/`MarkdownRenderer`) for the commonmark API.
- **Build**: `pom.xml` (compiler release 17, commonmark 0.30.0, new gfm-tables dependency, and checks on surefire/jacoco/javadoc/lombok against JDK 17). `.github/workflows/build.yml`, `release.yml` and `cve-scanning.yml` move from JDK 11 to JDK 17.
- **Consumers**: Java 8/11 users cannot upgrade to 2.x. Anyone who uses commonmark 0.15 classes through this jar's transitive dependency sees the newer commonmark version.
- **symphony-ai-platform**: no change in this change. Later, `ai-agent-messageml` can be deleted entirely and replaced by a dependency on `messageml-utils` 2.x, with an import rename from `com.symphony.ai.messageml` to `org.finos.symphony.messageml.messagemlutils.compose` (and `.prompt` to `.compose.format`). The platform re-adds its own tool pointer after the native-block section of the contract if it still wants one.
