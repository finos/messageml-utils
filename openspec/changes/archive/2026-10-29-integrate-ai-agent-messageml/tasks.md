# Tasks

Source root used below: `SRC=~/local/caip/symphony-ai-platform/ai-agent-messageml/src`. Target packages: `org.finos.symphony.messageml.messagemlutils.compose`, `...compose.parser` and `...compose.format`.

## 1. Java 17 baseline

- [x] 1.1 In `pom.xml`, replace `maven.compiler.source/target` 1.8 with `maven.compiler.release` 17. Check that lombok, surefire, jacoco, javadoc, source and gpg plugin versions support JDK 17. Verify that `./mvnw -B verify` passes on JDK 17 with no source change and the existing suite is green.
- [x] 1.2 Move `.github/workflows/build.yml`, `release.yml` and `cve-scanning.yml` from `java-version: 11` to `17`. Verify by grepping the workflows for `java-version` and checking that only `17` remains.
- [x] 1.3 Check that the javadoc and source jars still build under the release profile on JDK 17 (`./mvnw -B -DskipTests package` with the release profile's javadoc/source plugins enabled, or `javadoc:jar source:jar`). Verify that both jars are produced.

## 2. commonmark upgrade (existing behavior unchanged)

- [x] 2.1 Bump `commonmark.version` to 0.30.0 and add `org.commonmark:commonmark-ext-gfm-tables:${commonmark.version}`. Verify that `./mvnw dependency:tree -Dincludes=org.commonmark` shows both artifacts once at 0.30.0.
- [x] 2.2 Adapt `markdown/EntityDelimiterProcessor.java` (and any other class in `markdown/` that fails to compile) to the commonmark 0.30 `DelimiterProcessor` API. Verify that `./mvnw -B compile` succeeds.
- [x] 2.3 Run the full existing suite. Verify that it passes with zero changes to expected values in `src/test`. If any expected value differs, stop and report the diff instead of re-baselining (design D3 risk).

## 3. Port the commonmark parser extensions

- [x] 3.1 Copy `$SRC/main/java/com/symphony/ai/messageml/parser/*.java` (7 files) into `src/main/java/org/finos/symphony/messageml/messagemlutils/compose/parser/`, rewriting `package`/`import` from `com.symphony.ai.messageml` to `org.finos.symphony.messageml.messagemlutils.compose`. Verify that `./mvnw -B compile` succeeds.
- [x] 3.2 Port `$SRC/test/java/com/symphony/ai/messageml/parser/MessageMLBlockParserTest.java` to the matching test package. Verify that `./mvnw -B test -Dtest=MessageMLBlockParserTest` passes.

## 4. Port the validation and repair pipeline

- [x] 4.1 Copy `HtmlEntities`, `MessageMLTagScanner`, `MessageMLSanitizer`, `MessageMLBalancer`, `MessageMLStructureRepairer`, `MessageMLValidator`, `NativeMessageMLRenderer` and the records they use (`MessageMLRenderOptions`) into `.../messagemlutils/compose/`, with the package rename. Verify that `./mvnw -B compile` succeeds.
- [x] 4.2 Port `MessageMLTagScannerTest`, `MessageMLSanitizerTest`, `MessageMLBalancerTest`, `MessageMLStructureRepairerTest`, `MessageMLValidatorTest` and `NativeMessageMLRendererTest`. Verify that each passes with `./mvnw -B test -Dtest=<Class>`.
- [x] 4.3 Rewrite the jar scan in `MessageMLSanitizerTest` (renamed to `symphonyElementsAllowlist_isDerivedFromTheLibraryElements`) so it lists `FormElement` subclasses from a directory code source (`target/classes`) as well as a jar (design D5). Keep the `assertEquals(derived, SYMPHONY_ELEMENTS)` assertion. Verify that the test passes and fails if one tag is removed from `SYMPHONY_ELEMENTS` locally.
- [x] 4.4 Copy `$SRC/test/resources/messageml-doc-corpus/` to `src/test/resources/compose/messageml-doc-corpus/`, port `parser/MessageMLDocCorpusTest` and `MessageMLBalancerCorpusPropertiesTest`, and update resource paths. Verify that both pass and that the dynamic test count matches the number of `.mml` files.

## 5. Port the Markdown renderer and helpers

- [x] 5.1 Copy `MarkdownToMessageMLConverter`, `RenderedMessageML`, `FormCounts`, `SymphonyLinks`, `SymphonySyntaxNormalizer` and `StoredMessageText` into `.../messagemlutils/compose/`, with the package rename. Verify that `./mvnw -B compile` succeeds.
- [x] 5.2 Port `MarkdownToMessageMLConverterTest`, `SymphonyEntitySyntaxTest`, `ContainerDirectiveTest`, `MessageMLRenderOptionsTest`, `SymphonyLinksTest`, `SymphonySyntaxNormalizerTest` and `StoredMessageTextTest`. Verify that each passes.
- [x] 5.3 Copy `$SRC/test/resources/native-messageml-fixtures/` to `src/test/resources/compose/native-messageml-fixtures/`, port `MarkdownToMessageMLConverterNativeBlocksTest`, `NativeMessageMLFixturesTest` and `NativeMessageMLRepairPropertiesTest`, and update resource paths. Verify that all three pass.

## 6. Port the format description

- [x] 6.1 Copy `$SRC/main/java/com/symphony/ai/messageml/prompt/OutputFormatContract.java` and `SymphonyContentFormatGuide.java` into `src/main/java/org/finos/symphony/messageml/messagemlutils/compose/format/`, with the package rename. Move `$SRC/main/resources/symphony-content-format-guide.md` to `src/main/resources/org/finos/symphony/messageml/messagemlutils/compose/format/symphony-content-format-guide.md` and update `SymphonyContentFormatGuide.RESOURCE` to that absolute classpath path (design D4). Verify that `./mvnw -B compile` succeeds.
- [x] 6.2 In `OutputFormatContract.NATIVE_BLOCK`, replace the sentence `Call Symphony_ContentFormat_getContentFormatGuide once before your first block to see the full rules and the supported elements, then write:` with `Read the content format guide once before your first block to see the full rules and the supported elements, then write:`. Leave `BLOCK` byte-identical to the source. Verify with `diff <(sed -n '/BLOCK = """/,/""";/p' $SRC/.../OutputFormatContract.java) <(same on the ported file)` that only the one `NATIVE_BLOCK` line differs.
- [x] 6.3 Port `OutputFormatContractTest` and `SymphonyContentFormatGuideTest` into the matching test package. Verify that both pass, and that the guide test's dynamic test count equals the number of `<messageML>` openings in the guide resource.

## 7. Public-repo hygiene and docs

- [x] 7.1 Strip internal references from the ported Javadoc, test comments and text (`CAIP-*`, `caip-*`, `design.md` pointers, `DEC-*`, `D5 / §14`, benchmark percentages and prompt-skeleton/chat-memory narrative in `OutputFormatContract`, and "ai-agent-supervisor"/"fluxnova"/"Symphony_ContentFormat_getContentFormatGuide" where they describe platform wiring), keeping the behavioral explanation and the three contract invariants reworded as library guarantees (design D4, D6). Also clean `SOURCES.md`. Verify that `grep -rniE 'caip|design\.md|DEC-[0-9]|§|supervisor|fluxnova|Symphony_ContentFormat|SpEL|%\s*canonical' src/main/java/org/finos/symphony/messageml/messagemlutils/compose src/main/resources/org/finos/symphony/messageml/messagemlutils/compose src/test/java/org/finos/symphony/messageml/messagemlutils/compose src/test/resources/compose` returns nothing.
- [x] 7.2 Add Apache 2.0 license headers to the new files if the repo's existing sources carry them. Verify by comparing with `MessageMLContext.java`.
- [x] 7.3 Add a README section for the `compose` package: Java 17 requirement, a `MarkdownToMessageMLConverter` + `MessageMLRenderOptions` usage example, the validator/repair entry points, and a short paragraph on `OutputFormatContract` and `SymphonyContentFormatGuide` as the dialect's reference text. Also update `AGENTS.md` (Java 1.8 to 17, new package in the structure tree). Verify that the README snippet compiles by copying it into a scratch test and running it.

## 8. Integration check

- [x] 8.1 Run `./mvnw -B --file pom.xml verify` on JDK 17. Verify that the whole suite (existing plus ported) is green, that the JaCoCo report includes the `compose` packages, and that `javap -v` on a class in the built jar shows major version 61.
- [x] 8.2 Compare test counts: the number of ported test methods equals the number in `$SRC/test/java` (`grep -rcE '@(Test|ParameterizedTest|TestFactory)'`). Verify that the numbers match.
- [x] 8.3 Verify that the built jar contains `org/finos/symphony/messageml/messagemlutils/compose/format/symphony-content-format-guide.md` and no resource at the jar root besides those the library already shipped (`unzip -l target/*.jar | grep -v '/'`).
- [x] 8.4 Run `openspec validate integrate-ai-agent-messageml --strict` and check that it passes.
