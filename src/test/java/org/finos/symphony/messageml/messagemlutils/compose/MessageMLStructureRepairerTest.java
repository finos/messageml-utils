package org.finos.symphony.messageml.messagemlutils.compose;

import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.util.NoOpDataProvider;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

/**
 * Hand-written cases for {@link MessageMLStructureRepairer}, one per rule, plus three properties over
 * the documented corpus: a valid example is never touched, the repair is idempotent and every broken
 * case here comes out as valid MessageML.
 */
class MessageMLStructureRepairerTest {

  private final MessageMLStructureRepairer repairer = new MessageMLStructureRepairer();

  // ---------------------------------------------------------------------------------------------
  // Closing tags
  // ---------------------------------------------------------------------------------------------

  @Test
  void closingTagMissingInTheMiddle_closesTheInnerElementFirst() {
    assertEquals("<p><b>world</b></p>", repairer.repair("<p><b>world</p>"));
  }

  @Test
  void closingTagsInTheWrongOrder_areReordered() {
    assertEquals("<b><i>x</i></b> y", repairer.repair("<b><i>x</b></i> y"));
  }

  @Test
  void closingTagMatchingNothingOpen_isDropped() {
    assertEquals("<p>ab</p>", repairer.repair("<p>a</div>b</p>"));
  }

  @Test
  void mistypedCloseOfTheInlineElementOnTop_closesIt() {
    assertEquals("<i>x</i> y", repairer.repair("<i>x</b> y"));
  }

  @Test
  void cardClosedWithItsBodyAndInlineElementsStillOpen_closesThemAll() {
    assertEquals("<card><header>H</header><body><b>text</b></body></card>",
        repairer.repair("<card><header>H</header><body><b>text</p></card>"));
  }

  // ---------------------------------------------------------------------------------------------
  // Implied end tags
  // ---------------------------------------------------------------------------------------------

  @Test
  void bodyOpenedWhileTheHeaderIsStillOpen_closesTheHeader() {
    assertEquals("<card><header>Title</header><body>x</body></card>",
        repairer.repair("<card><header>Title<body>x</body></card>"));
  }

  @Test
  void headerClosedLateAfterTheBody_isNotDuplicated() {
    assertEquals("<expandable-card state=\"collapsed\"><header>T</header><body>x</body></expandable-card>",
        repairer.repair("<expandable-card state=\"collapsed\"><header>T<body>x</body></header></expandable-card>"));
  }

  @Test
  void listItemsWithoutClosingTags_areClosedByTheNextItem() {
    assertEquals("<ul><li>a</li><li>b</li></ul>", repairer.repair("<ul><li>a<li>b</ul>"));
  }

  @Test
  void nestedListInsideAnItem_isLeftAlone() {
    String nested = "<ul><li>a<ul><li>b</li></ul></li></ul>";
    assertEquals(nested, repairer.repair(nested));
  }

  @Test
  void nestedListWrittenAsASiblingOfTheItems_movesIntoThePreviousItem() {
    assertEquals("<ul><li>a<ul><li>b</li></ul></li><li>c</li></ul>",
        repairer.repair("<ul><li>a</li><ul><li>b</li></ul><li>c</li></ul>"));
  }

  @Test
  void blockAfterAListThatWasNeverClosed_closesTheList() {
    assertEquals("<ul><li>a</li></ul><p>after</p>", repairer.repair("<ul><li>a</li><p>after</p>"));
  }

  @Test
  void textWrittenStraightInsideAList_getsItsListItem() {
    assertEquals("<ul><li>a</li><li>b</li></ul>", repairer.repair("<ul>a<li>b</li></ul>"));
  }

  @Test
  void rowsAndCellsWithoutClosingTags_areClosedByTheNextOne() {
    assertEquals("<table><tr><td>a</td><td>b</td></tr><tr><td>c</td></tr></table>",
        repairer.repair("<table><tr><td>a<td>b<tr><td>c</table>"));
  }

  @Test
  void tableSectionClosedWithARowStillOpen_closesTheRow() {
    assertEquals("<table><thead><tr><th>A</th></tr></thead><tbody><tr><td>1</td></tr></tbody></table>",
        repairer.repair("<table><thead><tr><th>A</th></thead><tbody><tr><td>1</td></tr></table>"));
  }

  @Test
  void cellWrittenStraightInsideATable_getsItsRow() {
    assertEquals("<table><tr><td>x</td></tr></table>", repairer.repair("<table><td>x</td></table>"));
  }

  @Test
  void rowWrittenWithoutItsTable_getsATable() {
    assertEquals("<table><tr><td>a</td></tr><tr><td>b</td></tr></table>",
        repairer.repair("<tr><td>a</td></tr><tr><td>b</td></tr>"));
  }

  @Test
  void tableSectionWrittenWithoutItsTable_getsATable() {
    assertEquals("<table><tbody><tr><td>a</td></tr></tbody></table>",
        repairer.repair("<tbody><tr><td>a</td></tr></tbody>"));
  }

  @Test
  void cellsAtTheRoot_getARowAndATable() {
    assertEquals("<table><tr><td>a</td><td>b</td></tr></table>", repairer.repair("<td>a</td><td>b</td>"));
  }

  @Test
  void rowAfterATableThatWasClosed_getsItsOwnTable() {
    assertEquals("<table><tr><td>a</td></tr></table><table><tr><td>b</td></tr></table>",
        repairer.repair("<table><tr><td>a</td></tr></table><tr><td>b</td></tr>"));
  }

  @Test
  void captionWrittenBeforeTheFirstRow_getsARowOfItsOwnAndTheTableStaysOpen() {
    assertEquals("<table><tr><td>Table 1: users</td></tr><tr><td>User</td></tr></table>",
        repairer.repair("<table>Table 1: users<tr><td>User</td></tr></table>"));
  }

  @Test
  void captionWrittenInsideAnEmptyTableSection_getsARowInsideIt() {
    assertEquals("<table><tbody><tr><td>note</td></tr><tr><td>a</td></tr></tbody></table>",
        repairer.repair("<table><tbody>note<tr><td>a</td></tr></tbody></table>"));
  }

  @Test
  void captionWithLineBreaksAroundIt_isValidMessageML() {
    assertValid(repairer.repair("<table>\n  Table 1: User summary\n  <tr><td>User</td></tr>\n</table>"));
  }

  @Test
  void blockAfterATableThatWasNeverClosed_closesTheTable() {
    assertEquals("<table><tr><td>x</td></tr></table><p>after</p>",
        repairer.repair("<table><tr><td>x</td></tr><p>after</p>"));
  }

  @Test
  void paragraphOpenedInsideAnOpenParagraph_closesIt() {
    assertEquals("<p>a <b>b</b></p><p>c</p>", repairer.repair("<p>a <b>b<p>c</p>"));
  }

  @Test
  void headingOpenedInsideAnOpenHeading_closesIt() {
    assertEquals("<h1>a</h1><h2>b</h2>", repairer.repair("<h1>a<h2>b</h2></h1>"));
  }

  // ---------------------------------------------------------------------------------------------
  // Block content inside phrasing-only elements
  // ---------------------------------------------------------------------------------------------

  @Test
  void blockInsideBold_splitsTheBoldAroundIt() {
    assertEquals("<b>a</b><p><b>b</b></p><b>c</b>", repairer.repair("<b>a<p>b</p>c</b>"));
  }

  @Test
  void blockInsideNestedFormatting_reopensBothInTheirOriginalOrder() {
    assertEquals("<b><i>a</i></b><div><b><i>b</i></b></div>", repairer.repair("<b><i>a<div>b</div></i></b>"));
  }

  @Test
  void listInsideItalic_reopensTheItalicInsideTheItems() {
    assertEquals("<i>intro</i><ul><li><i>a</i></li></ul>", repairer.repair("<i>intro<ul><li>a</li></ul></i>"));
  }

  @Test
  void blockInsideAHeading_closesTheHeadingWithoutReopeningIt() {
    assertEquals("<h2>Title</h2><p>text</p>", repairer.repair("<h2>Title<p>text</p></h2>"));
  }

  @Test
  void codeInsidePre_dropsThePreAndKeepsTheCode() {
    String repaired = repairer.repair("<pre><code>x = 1</code></pre>");
    assertEquals("<code>x = 1</code>", repaired);
    assertValid(repaired);
  }

  @Test
  void codeInsidePre_keepsTheLanguageAndTheLineBreaks() {
    String repaired = repairer.repair("<pre>\n<code language=\"java\">int x;\nint y;</code>\n</pre>");
    assertEquals("\n<code language=\"java\">int x;\nint y;</code>\n", repaired);
    assertValid(repaired);
  }

  @Test
  void codeInsidePreAfterText_keepsThePreAndOpensTheCodeAfterIt() {
    String repaired = repairer.repair("<pre>a<code>b</code></pre>");
    assertEquals("<pre>a</pre><code>b</code>", repaired);
    assertValid(repaired);
  }

  @Test
  void preInsideCode_isLeftWhereItIs() {
    String nested = "<code><pre>x = 1</pre></code>";
    assertEquals(nested, repairer.repair(nested));
  }

  // ---------------------------------------------------------------------------------------------
  // Void and empty elements
  // ---------------------------------------------------------------------------------------------

  @Test
  void voidElementsWithoutTrailingSlash_areSelfClosedWherever_theySit() {
    assertEquals("<p>a<br/>b<hr/>c<img src=\"https://x.com/a.png\"/></p>",
        repairer.repair("<p>a<br>b<hr>c<img src=\"https://x.com/a.png\"></p>"));
  }

  @Test
  void voidElementWithItsOwnClosingTag_isLeftAlone() {
    String paired = "<p><img src=\"https://x.com/a.png\"></img></p>";
    assertEquals(paired, repairer.repair(paired));
  }

  @Test
  void strayVoidClosingTag_isDropped() {
    assertEquals("<p>ab</p>", repairer.repair("<p>a</br>b</p>"));
  }

  @Test
  void explicitlyEmptyList_isRemoved() {
    assertEquals("<p>a</p>", repairer.repair("<p>a</p><ul>\n</ul>"));
  }

  // ---------------------------------------------------------------------------------------------
  // Text
  // ---------------------------------------------------------------------------------------------

  @Test
  void rawLessThanAndStrayAmpersand_areEscaped() {
    assertEquals("<p>a &lt; b &amp; c, if x&lt;y then z</p>", repairer.repair("<p>a < b & c, if x<y then z</p>"));
  }

  @Test
  void htmlNamedReferences_becomeNumericAndXmlOnesAreKept() {
    assertEquals("<p>&#160;&#8212;&#8594;&amp;&lt;&#39;&#x2014;&amp;bogus;</p>",
        repairer.repair("<p>&nbsp;&mdash;&rarr;&amp;&lt;&#39;&#x2014;&bogus;</p>"));
  }

  @Test
  void commentsAreDroppedAndCdataBecomesEscapedText() {
    assertEquals("<p>abx&lt;y &amp; z</p>", repairer.repair("<p>a<!-- note -->b<![CDATA[x<y & z]]></p>"));
  }

  @Test
  void cdataKeepsTheSanitizerEscapesReadable() {
    assertEquals("<p>&#36;{x}</p>", repairer.repair("<p><![CDATA[&#36;{x}]]></p>"));
  }

  // ---------------------------------------------------------------------------------------------
  // Names
  // ---------------------------------------------------------------------------------------------

  @Test
  void upperCaseNames_areLowerCasedAndCamelCaseOnesRestored() {
    assertEquals("<b>x</b><dateTime value=\"2024-12-31T09:29:47Z\"/>",
        repairer.repair("<B>x</B><DATETIME value=\"2024-12-31T09:29:47Z\"/>"));
  }

  @Test
  void htmlFormattingElements_areMappedToTheirMessageMLEquivalents() {
    assertEquals("<b>a</b><i>b</i><span style=\"text-decoration: underline;\">c</span>"
            + "<span style=\"text-decoration: line-through;\">d</span>",
        repairer.repair("<strong>a</strong><em>b</em><u>c</u><del>d</del>"));
  }

  @Test
  void aliasStyle_isPrependedToAnExistingStyle() {
    assertEquals("<span style=\"text-decoration: underline; color:red;\">x</span>",
        repairer.repair("<u style=\"color:red;\">x</u>"));
  }

  @Test
  void aliasClosingAnElementOpenedUnderItsOtherName_isAMatch() {
    assertEquals("<b>x</b>", repairer.repair("<b>x</strong>"));
  }

  @Test
  void nestedMessageMLRootTags_areDropped() {
    assertEquals("<p>ab</p>", repairer.repair("<p>a<messageML>b</p>"));
  }

  // ---------------------------------------------------------------------------------------------
  // Attributes
  // ---------------------------------------------------------------------------------------------

  @Test
  void unquotedAttributeValue_isQuoted() {
    assertEquals("<card accent=\"tempo-bg-color--blue\"><body>x</body></card>",
        repairer.repair("<card accent=tempo-bg-color--blue><body>x</body></card>"));
  }

  @Test
  void strayAmpersandInAUrl_isEscaped() {
    assertEquals("<a href=\"https://x.com/?a=1&amp;b=2\">l</a>",
        repairer.repair("<a href=\"https://x.com/?a=1&b=2\">l</a>"));
  }

  @Test
  void duplicateAttribute_keepsTheFirstOne() {
    assertEquals("<div style=\"color:red;\">x</div>", repairer.repair("<div style=\"color:red;\" style=\"color:blue;\">x</div>"));
  }

  @Test
  void valuelessAttribute_isDropped() {
    assertEquals("<div>x</div>", repairer.repair("<div hidden>x</div>"));
  }

  @Test
  void valuelessFormBooleans_becomeTrue() {
    assertEquals("<checkbox name=\"c\" value=\"v\" checked=\"true\">x</checkbox>",
        repairer.repair("<checkbox name=\"c\" value=\"v\" checked>x</checkbox>"));
    assertEquals("<select name=\"s\" required=\"true\"><option value=\"1\" selected=\"true\">one</option></select>",
        repairer.repair("<select name=\"s\" REQUIRED><option value=\"1\" selected>one</option></select>"));
  }

  @Test
  void valuelessFormBoolean_keepsTheValueTheModelAlsoGave() {
    assertEquals("<checkbox name=\"c\" value=\"v\" checked=\"false\">x</checkbox>",
        repairer.repair("<checkbox name=\"c\" value=\"v\" checked checked=\"false\">x</checkbox>"));
  }

  @Test
  void valuelessFormBooleans_makeAValidForm() {
    assertValid(repairer.repair(
        "<form id=\"f\"><checkbox name=\"c\" value=\"v\" checked>x</checkbox>"
            + "<button name=\"b\" type=\"action\">go</button></form>"));
  }

  @Test
  void quoteTheTagScannerPairedButTheAttributeLoopCannot_runsToTheEndOfTheTag() {
    assertEquals("<span x=\"y&quot; z\">t</span>", repairer.repair("<span \"x='y\" z>t</span>"));
  }

  @Test
  void singleQuotedAttribute_isKeptAsWrittenWhenNothingIsWrong() {
    String fragment = "<card accent='tempo-bg-color--blue'><body>x</body></card>";
    assertEquals(fragment, repairer.repair(fragment));
  }

  @Test
  void emojiShortcodeWrittenWithColons_losesThem() {
    assertEquals("<p><emoji shortcode=\"rocket\"/></p>", repairer.repair("<p><emoji shortcode=\":rocket:\"/></p>"));
  }

  @Test
  void emojiShortcodeWithCharactersMessageMLRejects_isNormalized() {
    assertEquals("<emoji shortcode=\"thumbs_up\"/><emoji shortcode=\"+1\"/>",
        repairer.repair("<emoji shortcode=\"thumbs up!\"/><emoji shortcode=\":+1:\"/>"));
  }

  @Test
  void anchorWithoutDestination_isUnwrappedAndKeepsItsText() {
    assertEquals("<p>see the docs and <a href=\"https://x.com\">this</a></p>",
        repairer.repair("<p>see <a>the docs</a> and <a href=\"https://x.com\">this</a></p>"));
  }

  @Test
  void semanticElementsMissingTheirRequiredAttribute_areDropped() {
    assertEquals("<p>a  b  c  d  e</p>",
        repairer.repair("<p>a <img> b <emoji/> c <hash tag=\"\"/> d <mention/> e<dateTime/></p>"));
  }

  @Test
  void imageWithoutSourceButWithItsOwnClosingTag_isDroppedWhole() {
    assertEquals("<p>ab</p>", repairer.repair("<p>a<img></img>b</p>"));
  }

  // ---------------------------------------------------------------------------------------------
  // Truncation (streaming) - the same contract as MessageMLBalancer
  // ---------------------------------------------------------------------------------------------

  @Test
  void freshlyOpenedListItemAtTheEnd_isPruned() {
    assertEquals("<ul><li>A</li></ul>", repairer.repair("<ul><li>A</li><li>"));
  }

  @Test
  void cutInsideATag_dropsIt() {
    assertEquals("<p>Intro</p>", repairer.repair("<p>Intro</p><div style=\"border:1px"));
  }

  @Test
  void emptyNestedList_isRemovedWithoutDisturbingTheItemsAroundIt() {
    assertEquals("<ul><li>a</li><li>b</li></ul>", repairer.repair("<ul><li>a</li><li><ul>\n</ul>b</li></ul>"));
  }

  /** Probed against messageml-utils 1.5.0: unlike XHTML it accepts blocks in these, so they are not closed. */
  @Test
  void blockInsideAParagraphSpanOrAnchor_isLeftWhereItIs() {
    for (String fragment : new String[] {
        "<p>Steps: <ul><li>one</li></ul></p>",
        "<span>Steps: <table><tr><td>one</td></tr></table></span>",
        "<a href=\"https://x.com\">see <div>one</div></a>"}) {
      assertEquals(fragment, repairer.repair(fragment));
      assertValid(fragment);
    }
  }

  @Test
  void brokenAndCutShort_isRepairedAndClosed() {
    assertEquals("<p><b>x</b></p><p>y</p>", repairer.repair("<p><b>x</p><p>y"));
  }

  @Test
  void nullFragment_returnsNull() {
    assertNull(repairer.repair(null));
  }

  // ---------------------------------------------------------------------------------------------
  // Properties
  // ---------------------------------------------------------------------------------------------

  static Stream<String> brokenFragments() {
    return Stream.of(
        "<p><b>world</p>",
        "<b><i>x</b></i> y",
        "<card accent=\"tempo-bg-color--blue\"><header>Broken card</header><body><b>Unclosed bold</p></card>",
        "<card><header>First broken card<body>Missing header close tag.</body></card>",
        "<table><thead><tr><td>A</td><td>B</td></thead><tbody><tr><td>1</td></tr></table>",
        "<p>Balance due: &#36;{amount} <b>overdue</p>",
        "<ul><li>a<li>b<ul><li>c</ul></ul>",
        "<b>a<p>b</p>c</b>",
        "<h1>a<h2>b</h2></h1>",
        "<p>a<br>b &nbsp; c < d & e</p>",
        "<table><td>x</td><td>y</table><p>after",
        "<STRONG>Note</STRONG>: <u>read</u> <em>this</em><!-- x --><![CDATA[1<2]]>",
        "<ul><li>a</li><p>b</p>",
        "<card accent=tempo-bg-color--red><header>H</header><body>x</card>",
        "<li>orphan item<li>second</ol><h2>Title</h3><tr>cell without td</tr>",
        "<p>see <a>docs</a> <emoji shortcode=\"thumbs up\"/><img></p>",
        "<table>Table 1: users<tr><td>User</td></tr></table>",
        "<tr><td>a</td></tr><tr><td>b</td></tr>",
        "<td>a</td><td>b</td>",
        "<form id=\"f\"><checkbox name=\"c\" value=\"v\" checked>x</checkbox><button name=\"b\" type=\"action\">go</button></form>");
  }

  @ParameterizedTest
  @MethodSource("brokenFragments")
  void brokenFragment_isRepairedIntoValidMessageML(String fragment) {
    assertValid(repairer.repair(fragment));
  }

  @ParameterizedTest
  @MethodSource("brokenFragments")
  void repair_isIdempotent(String fragment) {
    String once = repairer.repair(fragment);
    assertEquals(once, repairer.repair(once));
  }

  @TestFactory
  Stream<DynamicTest> renderCorpus_isLeftUnchanged() {
    return corpusFiles().map(file -> dynamicTest(file.getFileName().toString(), () -> {
      String fragment = Files.readString(file).strip();
      assertEquals(fragment, repairer.repair(fragment));
    }));
  }

  private static void assertValid(String fragment) {
    String document = "<messageML>" + fragment + "</messageML>";
    assertDoesNotThrow(() -> new MessageMLContext(new NoOpDataProvider()).parseMessageML(document, null, null),
        () -> "expected valid MessageML: " + document);
  }

  static Stream<Path> corpusFiles() {
    try {
      URL dirUrl = MessageMLStructureRepairerTest.class.getClassLoader().getResource("compose/messageml-doc-corpus/render");
      assertNotNull(dirUrl, "missing corpus directory: compose/messageml-doc-corpus/render");
      try (Stream<Path> listing = Files.list(Paths.get(dirUrl.toURI()))) {
        List<Path> files = listing
            .filter(p -> p.getFileName().toString().endsWith(".mml"))
            .sorted(Comparator.comparing(p -> p.getFileName().toString()))
            .toList();
        return files.stream();
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    } catch (URISyntaxException e) {
      throw new IllegalStateException(e);
    }
  }
}
