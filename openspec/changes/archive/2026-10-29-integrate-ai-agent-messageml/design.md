# Design

## Context

See proposal.md (Why) for the motivation. The constraints below shape the approach.

- **Source module**: `symphony-ai-platform/ai-agent-messageml`, package `com.symphony.ai.messageml` (and `.parser`, `.prompt`). It builds with Java 25 against `messageml-utils` 1.8.2 and commonmark 0.30.0. It uses Java 17 language features throughout: records (`MessageMLRenderOptions`, `FormCounts`, `RenderedMessageML`, `NativeMessageMLRenderer.Result`, the `MessageMLTagScanner` tokens), a sealed `Token` interface, text blocks, pattern-matching `instanceof`, `List.of`/`Set.of`/`Map.of`, `String.isBlank`/`strip`. Its only runtime dependencies are commonmark, commonmark-ext-gfm-tables and messageml-utils. slf4j is declared but not used.
- **commonmark API floor**: `SymphonyEntityInlineParser` implements `org.commonmark.parser.beta.InlineContentParser` / `InlineContentParserFactory`. That API exists from commonmark 0.23.0, and commonmark 0.23+ requires Java 11.
- **Target library**: Java 1.8 (`maven.compiler.source/target`), commonmark 0.15.2, JDK 11 in CI. Its existing `markdown` package (`MarkdownParser`, `MarkdownRenderer`, `EntityDelimiterProcessor`) builds on commonmark. `EntityDelimiterProcessor` implements `getDelimiterUse(DelimiterRun, DelimiterRun)`, and the `process(...)` signature that goes with it changed after 0.15.
- **Coupling that disappears**: `MessageMLValidator` matches the library's error message strings with regexes. `MessageMLSanitizer` hard-codes the list of Elements tags, and a test checks that list against the classes in the messageml-utils jar. `MessageMLStructureRepairer` follows the content model it observed in messageml-utils 1.5.0. `SymphonyContentFormatGuideTest` renders every example of the guide through `NativeMessageMLRenderer`. Once the code lives in the same repo, all of these are checked against the current code on every build.
- **Platform wiring inside library text**: `OutputFormatContract` names a platform tool (`Symphony_ContentFormat_getContentFormatGuide`) in its native-block section, and its Javadoc explains invariants in terms of the platform's prompt assembly (SpEL, `String.format`, benchmark results). The platform's only call sites are `OutputFormatContract.block(boolean)` and `SymphonyContentFormatGuide.text()`.

## Goals / Non-Goals

**Goals:**
- Port the code with as few changes as possible: the same algorithms and the same output, checked by the ported tests unchanged apart from package and import edits.
- Keep the public API shape (class names, method signatures, record accessors) so that the later platform migration is only an import rename.
- One commonmark version for the whole library.
- Keep the library generic: no package, capability or wording that ties it to AI agents or LLMs. The format description is documentation of a dialect, usable by any producer.

**Non-Goals:**
- Refactoring the ported code or sharing helpers with the existing `markdown` package.
- Exposing the new renderer through `MessageMLContext`.
- Javadoc rewrites beyond removing internal references.
- Rewording the format contract or the guide. Their text is ported byte for byte, except for one sentence (D4).

## Decisions

### D1. Raise the baseline to Java 17 (not backport to Java 8 or 11)
Use `maven.compiler.release=17` and move all three workflows to JDK 17.
- *Alternative, stay on Java 8*: commonmark would be capped at 0.21.0, so the inline entity parser would need a rewrite (for example as a post-parse text visitor). Every record, sealed type and text block would also need backporting. That is weeks of divergence from a tested source, with real risk of behavior drift.
- *Alternative, Java 11*: commonmark 0.30 works, but records, sealed types and text blocks still need backporting, for little gain since Java 11 is also out of mainstream support.
- Choice made with the user. The cost: consumers on Java 8 or 11 cannot upgrade, so the release is 2.0.0.

### D2. Package `org.finos.symphony.messageml.messagemlutils.compose`
All root-package classes move to `...messagemlutils.compose`, `parser/` moves to `...compose.parser`, and `prompt/` moves to `...compose.format`. "Compose" names what the package does for any caller: it composes MessageML from authored input (extended Markdown, native fragments) and makes the result sendable (sanitize, balance, repair, validate). The name says nothing about who the author is.
- *Alternative, `agent`*: the first draft of this change. Rejected because it ties a generic library to one kind of producer, and the code itself never mentions agents or LLMs.
- *Alternative, put it in the existing `markdown` package*: name clashes in spirit with `MarkdownParser`/`MarkdownRenderer` (a different, older Markdown dialect and API), and it mixes repair classes into a Markdown package.
- *Alternative, `render`*: misnames the repair half of the package.
- *Alternative, split into `render`/`repair` packages*: the coupling is bidirectional. `MarkdownToMessageMLConverter.escapeHtml` is package-private and used by `MessageMLSanitizer`, `MessageMLValidator` and `NativeMessageMLRenderer`, while the converter itself calls `NativeMessageMLRenderer`. A split would widen that helper (or move it into `HtmlEntities`). Cheap, but not needed now; the flat layout keeps all package-private members (`HtmlEntities`, `MarkdownToMessageMLConverter.escapeHtml`, `MessageMLStructureRepairer.canonicalName`/`normalizeText`, `SymphonyLinks.canonicalId`, `NativeMessageMLRenderer.PLACEHOLDER`) as they are.
- The `format` sub-package is separate because it has no package-private coupling with the rest (its test uses only public API) and because it holds text, not behavior.

### D3. Upgrade commonmark to 0.30.0 and adapt the existing `markdown` package
Use the same version as the source module, add `commonmark-ext-gfm-tables` 0.30.0, and fix compile breaks in `EntityDelimiterProcessor` (and anywhere else they appear) by moving to the current `DelimiterProcessor.process(DelimiterRun, DelimiterRun)` contract. The existing `MarkdownParser`/`MessageMLContext` tests and the `src/test/resources/payloads` and `examples` fixtures are the oracle: their expected values do not change (spec `library-baseline`).
- *Alternative, shade or relocate a second commonmark*: this avoids touching legacy code but doubles the jar weight and confuses consumers. Rejected.

### D4. Include the format description as `compose.format`, with one generalized sentence
`OutputFormatContract`, `SymphonyContentFormatGuide` and `symphony-content-format-guide.md` move in. The knowledge they hold (which forms the dialect has, which MessageML elements a native block may use, what is forbidden) is this library's knowledge and will stay in step with the renderer only if it lives next to it. The platform call sites (`block(boolean)`, `text()`) keep their signatures.
- The one sentence in the native-block section that reads "Call Symphony_ContentFormat_getContentFormatGuide once before your first block to see the full rules and the supported elements" becomes "Read the content format guide once before your first block to see the full rules and the supported elements". A platform that exposes the guide as a tool appends its own pointer after `block(true)`. No other wording changes: the base block stays byte-identical to the source, which is what its tests pin.
- The embedding invariants stay and are restated as library guarantees rather than platform facts: the base block contains no `#{` (safe inside expression-template engines such as SpEL), contains no `%` (safe inside `String.format`), and never names MessageML, HTML or XML. The tests that assert them are ported unchanged.
- The resource moves from the classpath root to `/org/finos/symphony/messageml/messagemlutils/compose/format/symphony-content-format-guide.md` so a library jar does not claim a root-level resource name. `SymphonyContentFormatGuide.RESOURCE` is updated; `load(String)` and `readAll(InputStream)` stay package-private for their tests.
- *Alternative, leave the package in the platform*: the first draft of this change. Rejected because the guide's examples are only testable against the renderer, and because a split leaves the dialect's description drifting from its implementation.
- *Alternative, parameterize the tool pointer* (`block(boolean, String guideReference)`): more API for one sentence. Rejected; appending on the caller's side is simpler.

### D5. Tests and fixtures come along, adapted only where the location changed
Port every test under `com.symphony.ai.messageml`, including `prompt/`, with its fixtures into `src/test/java/org/finos/symphony/messageml/messagemlutils/compose/**` and `src/test/resources/compose/{native-messageml-fixtures,messageml-doc-corpus}`. Update resource paths. Two tests need more than a rename:
- `MessageMLSanitizerTest.symphonyElementsAllowlist_isDerivedFromThePinnedMessagemlUtilsJar` scans the messageml-utils jar for `FormElement` subclasses. Inside this repo the classes are a directory (`target/classes`), so the scan must handle a directory code source too. The test is renamed accordingly. The assertion does not change.
- `SymphonyContentFormatGuideTest.load_missingResource_throwsIllegalState` passes a root-level path (`/does-not-exist.md`); it keeps working with the relocated resource because the path is still absent. No change, but noted because `RESOURCE` moves (D4).
- Tests that use Java text blocks need no change, because the test sources also compile with release 17.
The source tests use only JUnit 5 (api and params), which the library already has. No Spring or AssertJ is needed.

### D6. Javadoc and text cleanup is limited to internal references
Remove Jira keys (`CAIP-757`, `CAIP-1059`, `caip-1032`), `design.md` section pointers and `DEC-`/`D5 / §14`-style IDs, because they mean nothing in a public FINOS repo. Where a sentence explains why the code behaves a certain way, keep the explanation and drop only the reference. In `OutputFormatContract`, drop the benchmark percentages and the description of the platform's prompt skeleton and chat memory; keep the statement that the wording is a measured constant that should not be edited in passing, and keep the three invariants reworded as in D4. `SOURCES.md` in the doc corpus keeps its upstream doc URLs and drops the AI-platform change name.

### D7. Versioning
The release workflow sets the version from the git tag. Tag `2.0.0`. Add a README section on the new package and the Java 17 requirement.

## Risks / Trade-offs

- [commonmark 0.15 to 0.30 changes how the legacy `MarkdownParser` parses input, for example emphasis or link edge cases] → The existing tests are the oracle. Any expected-value diff is a stop-and-ask item, not something to silently re-baseline (task 2.x).
- [Downstream users who pull commonmark 0.15 transitively break on 0.30] → Documented in the release notes as a breaking change of 2.0.0. Users on 1.x are unaffected.
- [Java 17 locks out Java 8/11 consumers] → Keep 1.8.x as the maintenance line, with fixes cherry-picked to a `1.x` branch if needed.
- [JaCoCo, javadoc or lombok plugin versions misbehave on JDK 17] → Lombok 1.18.30 and current surefire support 17. Verify the full `./mvnw verify`, including the `release` profile javadoc and source plugins, early (task 1.x).
- [Validator regexes depend on exact library error strings] → This is now a benefit: any change to an error message in this repo fails `MessageMLValidatorTest` in the same build.
- [The platform loses its tool pointer in the native-block section] → The platform appends it after `block(true)` in its own prompt builder during the follow-up migration. Until then the platform keeps using its private module, so nothing changes for it.
- [Public API widens the library's maintenance surface, and now includes prose constants] → Accepted. The classes are already stable and tested, the guide's examples are checked against the renderer on every build, and the AI platform is the main maintainer.

## Migration Plan

1. Merge to `main` and tag `2.0.0`. The release workflow publishes to Maven Central.
2. Follow-up, in a separate change in `symphony-ai-platform`: bump `messageml-utils.version` to 2.0.0, repoint `ai-agent-supervisor`, `fluxnova-symphony-bpmn` and `spring-ai-mock` imports to `org.finos.symphony.messageml.messagemlutils.compose` (and `.compose.format` for the two format classes), re-add the guide tool pointer after `OutputFormatContract.block(true)` in `AgentPromptBuilder`, and delete `ai-agent-messageml`.
3. Rollback: consumers pin 1.8.2. The library change is additive apart from the baseline.

## Open Questions

- Whether the 1.x line needs a maintenance branch depends on who still consumes it on Java 8/11. This can be decided after 2.0.0 ships.
