package org.finos.symphony.messageml.messagemlutils.compose;

import org.finos.symphony.messageml.messagemlutils.compose.MessageMLTagScanner.Cdata;
import org.finos.symphony.messageml.messagemlutils.compose.MessageMLTagScanner.CloseTag;
import org.finos.symphony.messageml.messagemlutils.compose.MessageMLTagScanner.Comment;
import org.finos.symphony.messageml.messagemlutils.compose.MessageMLTagScanner.OpenTag;
import org.finos.symphony.messageml.messagemlutils.compose.MessageMLTagScanner.ScanResult;
import org.finos.symphony.messageml.messagemlutils.compose.MessageMLTagScanner.Text;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Direct unit tests of the tokenizer shared by {@link MessageMLSanitizer} and
 * {@link MessageMLBalancer}. Each test checks the token shape, not
 * just that scanning doesn't throw, because both consumers depend on exact offsets.
 */
class MessageMLTagScannerTest {

  private final MessageMLTagScanner scanner = new MessageMLTagScanner();

  @Test
  void nullFragment_isRejectedUpFront() {
    assertThrows(NullPointerException.class, () -> scanner.scan(null));
  }

  @Test
  void simpleTagsAndText_produceOneTokenEach() {
    ScanResult result = scanner.scan("<p>Hello</p>");

    assertFalse(result.truncated());
    assertEquals("<p>Hello</p>".length(), result.consumedLength());
    assertEquals(3, result.tokens().size());

    OpenTag open = assertInstanceOf(OpenTag.class, result.tokens().get(0));
    assertEquals("p", open.name());
    assertFalse(open.selfClosing());

    Text text = assertInstanceOf(Text.class, result.tokens().get(1));
    assertEquals("Hello", "<p>Hello</p>".substring(text.start(), text.end()));

    CloseTag close = assertInstanceOf(CloseTag.class, result.tokens().get(2));
    assertEquals("p", close.name());
  }

  @Test
  void selfClosingTag_isNotAClosingTag() {
    ScanResult result = scanner.scan("<br/>");

    assertEquals(1, result.tokens().size());
    OpenTag tag = assertInstanceOf(OpenTag.class, result.tokens().get(0));
    assertEquals("br", tag.name());
    assertTrue(tag.selfClosing());
  }

  @Test
  void selfClosingTagWithSpaceBeforeSlash_isStillSelfClosing() {
    ScanResult result = scanner.scan("<hr />");

    OpenTag tag = assertInstanceOf(OpenTag.class, result.tokens().get(0));
    assertEquals("hr", tag.name());
    assertTrue(tag.selfClosing());
  }

  @Test
  void greaterThanInsideAQuotedAttributeValue_doesNotCloseTheTag() {
    String fragment = "<div title=\"a > b\">x</div>";
    ScanResult result = scanner.scan(fragment);

    assertFalse(result.truncated());
    OpenTag tag = assertInstanceOf(OpenTag.class, result.tokens().get(0));
    assertEquals("div", tag.name());
    assertEquals(fragment.indexOf('>', fragment.indexOf("\">")) , tag.end() - 1);
    assertEquals(1, tag.attributeValueRanges().size());
    int[] range = tag.attributeValueRanges().get(0);
    assertEquals("a > b", fragment.substring(range[0], range[1]));
  }

  @Test
  void unquotedAttributeValueEndingInSlash_isNotSelfClosing() {
    String fragment = "<a href=https://symphony.com/>x</a>";
    ScanResult result = scanner.scan(fragment);

    assertFalse(result.truncated());
    assertEquals(3, result.tokens().size());
    OpenTag tag = assertInstanceOf(OpenTag.class, result.tokens().get(0));
    assertEquals("a", tag.name());
    assertFalse(tag.selfClosing());
    assertEquals("a", assertInstanceOf(CloseTag.class, result.tokens().get(2)).name());
  }

  @Test
  void slashAfterAnUnquotedAttributeValueAndWhitespace_isSelfClosing() {
    for (String fragment : new String[] {"<img src=x />", "<img src = x />", "<img src='x'/>", "<img src=\"x\" />"}) {
      OpenTag tag = assertInstanceOf(OpenTag.class, scanner.scan(fragment).tokens().get(0));

      assertTrue(tag.selfClosing(), fragment);
    }
  }

  @Test
  void singleQuotedAttributeValue_isRecognised() {
    String fragment = "<ui-action action='open-im'>x</ui-action>";
    ScanResult result = scanner.scan(fragment);

    OpenTag tag = assertInstanceOf(OpenTag.class, result.tokens().get(0));
    assertEquals("ui-action", tag.name());
    assertEquals(1, tag.attributeValueRanges().size());
    int[] range = tag.attributeValueRanges().get(0);
    assertEquals("open-im", fragment.substring(range[0], range[1]));
  }

  @Test
  void comment_isItsOwnToken() {
    String fragment = "a<!-- comment -->b";
    ScanResult result = scanner.scan(fragment);

    assertFalse(result.truncated());
    assertEquals(3, result.tokens().size());
    assertInstanceOf(Text.class, result.tokens().get(0));
    Comment comment = assertInstanceOf(Comment.class, result.tokens().get(1));
    assertEquals("<!-- comment -->", fragment.substring(comment.start(), comment.end()));
    assertInstanceOf(Text.class, result.tokens().get(2));
  }

  @Test
  void cdata_isItsOwnToken() {
    String fragment = "<![CDATA[ raw <not-a-tag> ]]>";
    ScanResult result = scanner.scan(fragment);

    assertFalse(result.truncated());
    assertEquals(1, result.tokens().size());
    Cdata cdata = assertInstanceOf(Cdata.class, result.tokens().get(0));
    assertEquals(fragment, fragment.substring(cdata.start(), cdata.end()));
  }

  @Test
  void unterminatedTag_isReportedAsTruncatedAtItsStart() {
    String fragment = "<p>Intro</p><div style=\"border:1px";
    ScanResult result = scanner.scan(fragment);

    assertTrue(result.truncated());
    assertEquals(fragment.indexOf("<div"), result.consumedLength());
    assertEquals(3, result.tokens().size());
  }

  @Test
  void unterminatedComment_isReportedAsTruncatedAtItsStart() {
    String fragment = "<!-- unterminated comment";
    ScanResult result = scanner.scan(fragment);

    assertTrue(result.truncated());
    assertEquals(0, result.consumedLength());
    assertEquals(0, result.tokens().size());
  }

  @Test
  void unterminatedCdata_isReportedAsTruncatedAtItsStart() {
    String fragment = "<![CDATA[ unterminated";
    ScanResult result = scanner.scan(fragment);

    assertTrue(result.truncated());
    assertEquals(0, result.consumedLength());
  }

  /** A final "AT&T" cannot be told from a stream that is about to type "&Tau;": keep it as text. */
  @Test
  void unterminatedTrailingEntityReference_staysText() {
    String fragment = "foo &am";
    ScanResult result = scanner.scan(fragment);

    assertFalse(result.truncated());
    assertEquals(fragment.length(), result.consumedLength());
    assertEquals(1, result.tokens().size());
    Text text = assertInstanceOf(Text.class, result.tokens().get(0));
    assertEquals(fragment, fragment.substring(text.start(), text.end()));
  }

  @Test
  void completeTrailingEntityReference_isNotTruncated() {
    String fragment = "foo &amp;";
    ScanResult result = scanner.scan(fragment);

    assertFalse(result.truncated());
    assertEquals(fragment.length(), result.consumedLength());
  }

  @Test
  void freemarkerDirective_capturesTheHashAsPartOfTheName() {
    ScanResult result = scanner.scan("<#assign x=1>");

    OpenTag tag = assertInstanceOf(OpenTag.class, result.tokens().get(0));
    assertEquals("#assign", tag.name());
  }

  @Test
  void freemarkerClosingDirective_capturesTheHashAsPartOfTheName() {
    ScanResult result = scanner.scan("</#if>");

    CloseTag tag = assertInstanceOf(CloseTag.class, result.tokens().get(0));
    assertEquals("#if", tag.name());
  }

  @Test
  void freemarkerMacroCall_capturesTheAtAsPartOfTheName() {
    ScanResult result = scanner.scan("<@macro/>");

    OpenTag tag = assertInstanceOf(OpenTag.class, result.tokens().get(0));
    assertEquals("@macro", tag.name());
    assertTrue(tag.selfClosing());
  }

  @Test
  void freemarkerClosingMacro_capturesTheAtAsPartOfTheName() {
    ScanResult result = scanner.scan("</@macro>");

    CloseTag tag = assertInstanceOf(CloseTag.class, result.tokens().get(0));
    assertEquals("@macro", tag.name());
  }

  // ---------------------------------------------------------------------------------------------
  // A '<' or '&' that cannot start markup stays text: treating it as a truncation used to cut the
  // rest of a final answer ("<p>if a < b then c</p>" rendered as "<p>if a </p>").
  // ---------------------------------------------------------------------------------------------

  @Test
  void lessThanThatCannotStartATag_isPartOfTheText() {
    String fragment = "<p>a < b, x<5</p>";
    ScanResult result = scanner.scan(fragment);

    assertFalse(result.truncated());
    assertEquals(3, result.tokens().size());
    Text text = assertInstanceOf(Text.class, result.tokens().get(1));
    assertEquals("a < b, x<5", fragment.substring(text.start(), text.end()));
    assertInstanceOf(CloseTag.class, result.tokens().get(2));
  }

  @Test
  void tagCandidateRunningIntoAnotherLessThan_isText() {
    String fragment = "<p>if a<b then c</p>";
    ScanResult result = scanner.scan(fragment);

    assertFalse(result.truncated());
    Text text = assertInstanceOf(Text.class, result.tokens().get(1));
    assertEquals("if a<b then c", fragment.substring(text.start(), text.end()));
    assertEquals("p", assertInstanceOf(CloseTag.class, result.tokens().get(2)).name());
  }

  @Test
  void lessThanInsideAnUnclosedQuotedValue_endsTheTagCandidateAndKeepsTheRest() {
    String fragment = "<p>ok</p><img src=\"x > <p>more</p>";
    ScanResult result = scanner.scan(fragment);

    assertFalse(result.truncated());
    assertEquals(fragment.length(), result.consumedLength());
    assertEquals(7, result.tokens().size());
    Text text = assertInstanceOf(Text.class, result.tokens().get(3));
    assertEquals("<img src=\"x > ", fragment.substring(text.start(), text.end()));
    assertEquals("p", assertInstanceOf(OpenTag.class, result.tokens().get(4)).name());
    assertEquals("p", assertInstanceOf(CloseTag.class, result.tokens().get(6)).name());
  }

  @Test
  void unclosedQuoteWithNoLessThanAfterIt_isStillTruncated() {
    String fragment = "<p>x</p><img alt=\"a > b";
    ScanResult result = scanner.scan(fragment);

    assertTrue(result.truncated());
    assertEquals("<p>x</p>".length(), result.consumedLength());
  }

  @Test
  void markupDeclarationThatIsNeitherACommentNorCdata_isText() {
    ScanResult result = scanner.scan("<!DOCTYPE html><p>x</p>");

    assertFalse(result.truncated());
    assertInstanceOf(Text.class, result.tokens().get(0));
    assertEquals("p", assertInstanceOf(OpenTag.class, result.tokens().get(1)).name());
  }

  @Test
  void lessThanThatCouldStillBecomeMarkupAtTheEnd_isTruncated() {
    for (String tail : new String[] {"<", "</", "<!", "<!-", "<![CD", "<![CDATA"}) {
      String fragment = "<p>x</p>" + tail;
      ScanResult result = scanner.scan(fragment);

      assertTrue(result.truncated(), tail);
      assertEquals("<p>x</p>".length(), result.consumedLength(), tail);
    }
  }

  @Test
  void tagNameWithNoClosingBracketYet_isTruncated() {
    ScanResult result = scanner.scan("<p>x</p><b clas");

    assertTrue(result.truncated());
    assertEquals("<p>x</p>".length(), result.consumedLength());
  }

  @Test
  void ampersandInProseAtTheEnd_isNotTruncated() {
    ScanResult result = scanner.scan("Profit & Loss");

    assertFalse(result.truncated());
    assertEquals("Profit & Loss".length(), result.consumedLength());
  }

  @Test
  void trailingAmpersandForms_areNeverTruncated() {
    for (String fragment : new String[] {"Profit &", "x &#12", "x &#x1F", "x &mdash", "AT&T", "R&D", "Q&A"}) {
      ScanResult result = scanner.scan(fragment);

      assertFalse(result.truncated(), fragment);
      assertEquals(fragment.length(), result.consumedLength(), fragment);
    }
  }
}
