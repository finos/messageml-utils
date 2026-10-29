package org.finos.symphony.messageml.messagemlutils.compose;

import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.util.NoOpDataProvider;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

/**
 * The per-block pipeline: sanitize, balance, validate, repair,
 * validate again, inline or degrade.
 */
class NativeMessageMLRendererTest {

  private final NativeMessageMLRenderer renderer = new NativeMessageMLRenderer();

  private static final MessageMLRenderOptions FINAL = MessageMLRenderOptions.full();
  private static final MessageMLRenderOptions INTERMEDIATE = MessageMLRenderOptions.full().asIntermediate();

  // ---------------------------------------------------------------------------------------------
  // Clean valid block
  // ---------------------------------------------------------------------------------------------

  @Test
  void cleanBlock_final_isRenderedUnchanged() {
    String body = "<p>Hello <b>world</b></p>";

    NativeMessageMLRenderer.Result result = renderer.render(body, FINAL);

    assertEquals(NativeMessageMLRenderer.Outcome.RENDERED, result.outcome());
    assertEquals(body, result.markup());
    assertValid(result.markup());
  }

  /** A final answer that stops on "AT&T" or "R&D" must keep every character of it. */
  @Test
  void finalBlockEndingOnAnAcronymWithAnAmpersand_keepsIt() {
    NativeMessageMLRenderer.Result result = renderer.render("<p>Ask the people in R&D", FINAL);

    assertEquals(NativeMessageMLRenderer.Outcome.RENDERED, result.outcome());
    assertEquals("<p>Ask the people in R&amp;D</p>", result.markup());
    assertValid(result.markup());
  }

  @Test
  void cleanBlock_intermediate_isRenderedUnchanged() {
    String body = "<p>Hello <b>world</b></p>";

    NativeMessageMLRenderer.Result result = renderer.render(body, INTERMEDIATE);

    assertEquals(NativeMessageMLRenderer.Outcome.RENDERED, result.outcome());
    assertEquals(body, result.markup());
    assertValid(result.markup());
  }

  // ---------------------------------------------------------------------------------------------
  // Repairable block: an attribute MessageML does not allow on expandable-card (MessageMLValidatorTest
  // #repair_attributeNotAllowedOnItsElement_isDropped verifies the library drops exactly this one)
  // ---------------------------------------------------------------------------------------------

  @Test
  void repairableBlock_final_isRenderedWithTheAttributeDropped() {
    String body = "<expandable-card state=\"collapsed\" accent=\"blue\">"
        + "<header>H</header><body>B</body></expandable-card>";

    NativeMessageMLRenderer.Result result = renderer.render(body, FINAL);

    assertEquals(NativeMessageMLRenderer.Outcome.REPAIRED, result.outcome());
    assertFalse(result.markup().contains("accent"), result.markup());
    assertTrue(result.markup().contains("<expandable-card state=\"collapsed\">"), result.markup());
    assertValid(result.markup());
  }

  @Test
  void repairableBlock_intermediate_isRenderedWithTheAttributeDroppedAndExpanded() {
    String body = "<expandable-card state=\"collapsed\" accent=\"blue\">"
        + "<header>H</header><body>B</body></expandable-card>";

    NativeMessageMLRenderer.Result result = renderer.render(body, INTERMEDIATE);

    assertEquals(NativeMessageMLRenderer.Outcome.REPAIRED, result.outcome());
    assertFalse(result.markup().contains("accent"), result.markup());
    assertTrue(result.markup().contains("state=\"expanded\""), result.markup());
    assertValid(result.markup());
  }

  // ---------------------------------------------------------------------------------------------
  // Sanitizer-rejected block
  // ---------------------------------------------------------------------------------------------

  @Test
  void sanitizerRejectedBlock_final_isDegradedToTheCollapsedCardWithTheOriginalSource() {
    String body = "<#assign x=1>";

    NativeMessageMLRenderer.Result result = renderer.render(body, FINAL);

    assertEquals(NativeMessageMLRenderer.Outcome.DEGRADED, result.outcome());
    assertTrue(result.markup().startsWith("<expandable-card state=\"collapsed\">"), result.markup());
    assertTrue(result.markup().contains("Rich content could not be displayed"), result.markup());
    assertTrue(result.markup().contains(MarkdownToMessageMLConverter.escapeHtml(body)), result.markup());
    // Nothing the model wrote is lost and it is never evaluated: the raw directive text only ever
    // appears escaped, inside <code>.
    assertFalse(result.markup().contains("<#assign"), result.markup());
    assertValid("<messageML>" + result.markup() + "</messageML>");
  }

  @Test
  void sanitizerRejectedBlock_intermediate_isExactlyThePlaceholder() {
    String body = "<#assign x=1>";

    NativeMessageMLRenderer.Result result = renderer.render(body, INTERMEDIATE);

    assertEquals(NativeMessageMLRenderer.Outcome.DEGRADED, result.outcome());
    assertEquals("<i>✨ Generating rich content…</i>", result.markup());
  }

  // ---------------------------------------------------------------------------------------------
  // Structurally broken block: a closing tag missing in the middle is refused by the balancer and
  // fixed by the structural repair.
  // ---------------------------------------------------------------------------------------------

  @Test
  void closingTagMissingInTheMiddle_final_isRepaired() {
    NativeMessageMLRenderer.Result result = renderer.render("<p><b>world</p>", FINAL);

    assertEquals(NativeMessageMLRenderer.Outcome.REPAIRED, result.outcome());
    assertEquals("<p><b>world</b></p>", result.markup());
  }

  @Test
  void closingTagMissingInTheMiddle_intermediate_isRepairedRatherThanShownAsThePlaceholder() {
    NativeMessageMLRenderer.Result result = renderer.render("<p><b>world</p><p>still stream", INTERMEDIATE);

    assertEquals(NativeMessageMLRenderer.Outcome.REPAIRED, result.outcome());
    assertEquals("<p><b>world</b></p><p>still stream</p>", result.markup());
  }

  @Test
  void upperCaseFormInABrokenBlock_isDegradedRatherThanLowerCasedIntoARealForm() {
    NativeMessageMLRenderer.Result result = renderer.render("<p>x</b><FORM><BUTTON name=\"b\">ok</BUTTON></FORM>", FINAL);

    assertEquals(NativeMessageMLRenderer.Outcome.DEGRADED, result.outcome());
    assertFalse(result.markup().contains("<form"), result.markup());
  }

  @Test
  void structurallyRepairedBlockWithABadAttribute_isAlsoRepairedByTheValidator() {
    NativeMessageMLRenderer.Result result = renderer.render("<p title=\"x\"><b>world</p>", FINAL);

    assertEquals(NativeMessageMLRenderer.Outcome.REPAIRED, result.outcome());
    assertEquals("<p><b>world</b></p>", result.markup());
  }

  @Test
  void unclosedAttributeQuoteFollowedByMarkup_keepsTheContentAfterIt() {
    String body = "<p>ok</p><img src=\"http://example.com/logo.png > <p>Here is my text</p>";

    NativeMessageMLRenderer.Result result = renderer.render(body, FINAL);

    assertEquals(NativeMessageMLRenderer.Outcome.REPAIRED, result.outcome());
    assertTrue(result.markup().startsWith("<p>ok</p>"), result.markup());
    assertTrue(result.markup().endsWith("<p>Here is my text</p>"), result.markup());
    assertValid(result.markup());
  }

  @Test
  void unquotedAttributeValueEndingInSlash_keepsTheElementAndItsText() {
    NativeMessageMLRenderer.Result result =
        renderer.render("<p><a href=https://symphony.com/>Symphony</a></p>", FINAL);

    assertEquals(NativeMessageMLRenderer.Outcome.REPAIRED, result.outcome());
    assertEquals("<p><a href=\"https://symphony.com/\">Symphony</a></p>", result.markup());
    assertValid(result.markup());
  }

  // ---------------------------------------------------------------------------------------------
  // Unrepairable block: a date the validator rejects is not a fault either repair stage rewrites.
  // ---------------------------------------------------------------------------------------------

  @Test
  void unrepairableBlock_final_isDegradedToTheCollapsedCardWithTheOriginalSource() {
    String body = "<p>Due <dateTime value=\"tomorrow\"/></p>";

    NativeMessageMLRenderer.Result result = renderer.render(body, FINAL);

    assertEquals(NativeMessageMLRenderer.Outcome.DEGRADED, result.outcome());
    assertTrue(result.markup().contains(MarkdownToMessageMLConverter.escapeHtml(body)), result.markup());
    assertValid("<messageML>" + result.markup() + "</messageML>");
  }

  @Test
  void unrepairableBlock_intermediate_isExactlyThePlaceholder() {
    String body = "<p>Due <dateTime value=\"tomorrow\"/></p>";

    NativeMessageMLRenderer.Result result = renderer.render(body, INTERMEDIATE);

    assertEquals(NativeMessageMLRenderer.Outcome.DEGRADED, result.outcome());
    assertEquals("<i>✨ Generating rich content…</i>", result.markup());
  }

  // ---------------------------------------------------------------------------------------------
  // expandable-card state: forced to expanded on intermediate, authored state kept on final
  // ---------------------------------------------------------------------------------------------

  @Test
  void expandableCardCollapsed_intermediate_isShownExpanded() {
    String body = "<expandable-card state=\"collapsed\"><header>H</header><body>B</body></expandable-card>";

    NativeMessageMLRenderer.Result result = renderer.render(body, INTERMEDIATE);

    assertEquals(NativeMessageMLRenderer.Outcome.RENDERED, result.outcome());
    assertTrue(result.markup().contains("state=\"expanded\""), result.markup());
    assertFalse(result.markup().contains("state=\"collapsed\""), result.markup());
  }

  @Test
  void expandableCardCollapsed_final_keepsItsAuthoredState() {
    String body = "<expandable-card state=\"collapsed\"><header>H</header><body>B</body></expandable-card>";

    NativeMessageMLRenderer.Result result = renderer.render(body, FINAL);

    assertEquals(NativeMessageMLRenderer.Outcome.RENDERED, result.outcome());
    assertEquals(body, result.markup());
    assertTrue(result.markup().contains("state=\"collapsed\""), result.markup());
  }

  // ---------------------------------------------------------------------------------------------
  // The "render" doc corpus: every example renders without a problem - never DEGRADED.
  // ---------------------------------------------------------------------------------------------

  @TestFactory
  Stream<DynamicTest> renderCorpus_neverDegrades() {
    return corpusFiles("render").map(file -> dynamicTest(file.getFileName().toString(), () -> {
      String fragment = readFragment(file);

      NativeMessageMLRenderer.Result result = renderer.render(fragment, FINAL);

      assertNotEquals(NativeMessageMLRenderer.Outcome.DEGRADED, result.outcome(),
          () -> file.getFileName() + " was expected to render or repair, not degrade: " + result.markup());
      assertFalse(result.markup().isBlank(), () -> file.getFileName() + " rendered blank content");
    }));
  }

  // ---------------------------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------------------------

  private static void assertValid(String markup) {
    String wrapped = markup.startsWith("<messageML>") ? markup : "<messageML>" + markup + "</messageML>";
    assertDoesNotThrow(() -> new MessageMLContext(new NoOpDataProvider()).parseMessageML(wrapped, null, null),
        () -> "expected valid MessageML: " + wrapped);
  }

  private static String readFragment(Path file) {
    try {
      return Files.readString(file).strip();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private static Stream<Path> corpusFiles(String bucket) {
    try {
      URL dirUrl = NativeMessageMLRendererTest.class.getClassLoader()
          .getResource("compose/messageml-doc-corpus/" + bucket);
      assertNotNull(dirUrl, () -> "missing corpus directory: compose/messageml-doc-corpus/" + bucket);
      Path dir = Paths.get(dirUrl.toURI());
      try (Stream<Path> listing = Files.list(dir)) {
        List<Path> files = listing
            .filter(p -> p.getFileName().toString().endsWith(".mml"))
            .sorted(Comparator.comparing(p -> p.getFileName().toString()))
            .toList();
        return files.stream();
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    } catch (URISyntaxException e) {
      throw new RuntimeException(e);
    }
  }
}
