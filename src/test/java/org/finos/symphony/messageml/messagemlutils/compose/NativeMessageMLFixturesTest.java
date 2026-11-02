package org.finos.symphony.messageml.messagemlutils.compose;

import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.util.NoOpDataProvider;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.params.provider.Arguments.arguments;

/**
 * Whole answers mixing Markdown and native {@code <messageML>} blocks, as a model writes them, run
 * through {@link MarkdownToMessageMLConverter} with native MessageML on. The same files can be sent
 * to a real room, so what is asserted here is what shows up in Symphony.
 */
class NativeMessageMLFixturesTest {

  private static final String FIXTURES = "/compose/native-messageml-fixtures/";
  private static final String DEGRADED_HEADER = "Rich content could not be displayed";
  private static final Pattern BLOCK = Pattern.compile("<messageML>(.*?)</messageML>", Pattern.DOTALL);

  private static final MessageMLRenderOptions FINAL = MessageMLRenderOptions.full().withNativeMessageML(true);
  private static final MessageMLRenderOptions INTERMEDIATE = FINAL.asIntermediate();

  private final MarkdownToMessageMLConverter converter = new MarkdownToMessageMLConverter();

  /**
   * Every fixture with its expected block outcomes and markup that must appear in the final
   * message. For a fixture whose blocks all render as written, every block body is checked verbatim
   * on top of that.
   */
  static Stream<Arguments> fixtures() {
    return Stream.of(
        arguments("01-jira-table-status.md", 1, 0, 0, List.of()),
        arguments("02-confluence-card.md", 1, 0, 0, List.of()),
        arguments("03-expandable-risks.md", 1, 0, 0, List.of()),
        arguments("04-deployment-checklist.md", 1, 0, 0, List.of()),
        arguments("05-styled-alert.md", 1, 0, 0, List.of()),
        arguments("06-structured-date.md", 1, 0, 0, List.of()),
        arguments("07-image-logo.md", 1, 0, 0, List.of()),
        arguments("08-hash-cash-mentions.md", 1, 0, 0, List.of()),
        arguments("09-emoji-celebration.md", 1, 0, 0, List.of()),
        arguments("10-broken-block-degrades.md", 0, 1, 0, List.of(
            "<card accent=\"tempo-bg-color--blue\">",
            "<header>Broken card</header>",
            "<body><b>Unclosed bold and mismatched tags\n</b></body></card>")),
        arguments("11-multiple-broken-blocks.md", 1, 1, 0, List.of(
            "<header>First broken card\n</header><body>Missing header close tag.</body>",
            "<p>Open paragraph with <b>bold that never closes</b></p>")),
        arguments("12-broken-and-valid-mixed.md", 2, 1, 0, List.of(
            "<card accent=\"tempo-bg-color--blue\">\n<header>Valid card</header>",
            "<tr><td>Column A</td><td>Column B</td>\n</tr></thead>",
            "<tr><td>Row 1, A</td></tr>\n</tbody></table>",
            "<p>Back to normal, filed under <hash tag=\"release\"/>.</p>")),
        arguments("13-unterminated-block.md", 1, 0, 0, List.of(
            "<body>There is no closing messageML tag after this line.</body></card>")),
        arguments("14-forbidden-form-block.md", 0, 0, 1, List.of(
            DEGRADED_HEADER,
            "&lt;form id=&quot;broken-form&quot;&gt;")),
        arguments("15-templating-and-mismatch-combo.md", 1, 1, 0, List.of(
            "<p>Balance due: &#36;{amount} <b>overdue</b></p>",
            "<div style=\"color:red;\">\n<span>Unclosed span inside an unclosed div</span></div>")),
        arguments("16-html-habits.md", 0, 1, 0, List.of(
            "<p><b>Heads up:</b> the <i>Q3 P&amp;L</i> report&#160;is out &#8212; see "
                + "<a href=\"https://example.com/report?q=3&amp;format=pdf\">the PDF</a>.<br/>",
            "Revenue &lt; forecast by 2%, margin > target.</p>",
            "<p><span style=\"text-decoration: underline;\">Owner:</span> finance team "
                + "<emoji shortcode=\"bar_chart\"/></p>")),
        arguments("17-forgotten-structure-tags.md", 0, 1, 0, List.of(
            "<tr><th>Id</th><th>Status</th>\n</tr><tr><td>INC-1</td><td>Investigating\n</td></tr>"
                + "<tr><td>INC-2</td><td>Resolved</td>\n</tr></table>",
            "<li>Pager rotation updated\n</li><li>Runbook reviewed\n<ul><li>Owner: SRE</li></ul>\n</li></ul>",
            "<p>Last refreshed <b>5 minutes ago</b></p>")),
        arguments("18-mistyped-closing-tags.md", 0, 1, 0, List.of(
            "<h2>Release notes</h2>",
            "<li>Native rich content</li>\n</ol>",
            "<header>Highlights\n</header><body><i>Broken blocks are now repaired instead of degraded.</i>\n"
                + "</body></card>")),
        arguments("19-bold-around-blocks.md", 0, 1, 0, List.of(
            "<b>Summary\n</b><p><b>All services are healthy.</b></p>",
            "<ul><li><b>API: 99.99%</b></li><li><b>Datafeed: 99.95%</b></li></ul>",
            "<b>\nNext review on Monday.</b>")));
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("fixtures")
  void finalRender_isValidAndEachBlockHasItsExpectedOutcome(String fixture, int rendered, int repaired,
      int degraded, List<String> expectedMarkup) {
    String markdown = readFixture(fixture);

    RenderedMessageML result = converter.render(markdown, FINAL);
    String document = "<messageML>" + result.body() + "</messageML>";

    assertValid(document);
    assertAll(
        () -> assertEquals(rendered, result.forms().nativeRendered(), () -> "rendered blocks in " + document),
        () -> assertEquals(repaired, result.forms().nativeRepaired(), () -> "repaired blocks in " + document),
        () -> assertEquals(degraded, result.forms().nativeDegraded(), () -> "degraded blocks in " + document),
        () -> assertEquals(degraded > 0, document.contains(DEGRADED_HEADER), document));
    for (String markup : expectedMarkup) {
      assertTrue(document.contains(markup), () -> "missing " + markup + " in " + document);
    }
    if (repaired == 0 && degraded == 0) {
      Matcher block = BLOCK.matcher(markdown);
      while (block.find()) {
        String body = block.group(1).strip();
        assertTrue(document.contains(body), () -> "block not inlined as written: " + body + " in " + document);
      }
    }
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("fixtures")
  void everyStreamingPrefix_rendersAValidDocument(String fixture) {
    String markdown = readFixture(fixture);
    for (int cut = 1; cut <= markdown.length(); cut += 7) {
      String prefix = markdown.substring(0, cut);
      String document = converter.renderDocument(prefix, INTERMEDIATE);
      assertValid(document);
      assertFalse(document.contains(DEGRADED_HEADER), () -> "degraded card on a streaming update: " + document);
    }
  }

  @Test
  void forbiddenFormBlock_neverRendersTheFormItself() {
    String document = converter.renderDocument(readFixture("14-forbidden-form-block.md"), FINAL);

    assertFalse(document.contains("<form"), document);
    assertFalse(document.contains("<button"), document);
    assertFalse(document.contains("<text-field"), document);
  }

  private static void assertValid(String document) {
    assertDoesNotThrow(() -> new MessageMLContext(new NoOpDataProvider()).parseMessageML(document, null, null),
        () -> "expected valid MessageML: " + document);
  }

  private static String readFixture(String name) {
    try (InputStream in = NativeMessageMLFixturesTest.class.getResourceAsStream(FIXTURES + name)) {
      return new String(Objects.requireNonNull(in, "missing fixture " + name).readAllBytes(), StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
