package org.finos.symphony.messageml.messagemlutils.compose;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

/**
 * The extended syntax is a contract between the producer and the model, so anything
 * that reads the model's words rather than the rendered message must not see it.
 */
class SymphonySyntaxNormalizerTest {

  @Test
  void expandDirective_becomesABoldLabelAndItsBody() {
    String markdown = """
        :::expand Detail
        Body here.
        :::""";

    assertEquals("**Detail**\nBody here.", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void cardDirective_withAHeading_keepsTheHeading() {
    String markdown = """
        :::card
        ### Risk summary
        Exposure is fine.
        :::""";

    assertEquals("### Risk summary\nExposure is fine.",
        SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void cardDirective_attributesAreNotWordsTheReaderWants() {
    String markdown = """
        :::card accent=blue icon=https://example.com/i.png
        Body.
        :::""";

    assertEquals("Body.", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void cardDirective_withATitleOnTheOpeningLine_keepsIt() {
    String markdown = """
        :::card accent=blue Risk summary
        Body.
        :::""";

    assertEquals("**Risk summary**\nBody.", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void expandModifier_isRemovedWithTheDirective() {
    assertEquals("**Detail**\nBody.",
        SymphonySyntaxNormalizer.toPlainMarkdown(":::expand[expanded] Detail\nBody.\n:::"));
  }

  @Test
  void entityForms_areReducedToTheirDisplayText() {
    String markdown = "Ask @[Jane](mailto:jane@x.com) about #[topic] and $[AAPL].";

    assertEquals("Ask Jane about topic and AAPL.", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void uidMentionForm_isReducedToItsName() {
    String markdown = "Ping [Ops Desk](sym:user/71811853189183) and @[Jane](mailto:jane@x.com).";

    assertEquals("Ping Ops Desk and Jane.", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void plainMailtoLink_keepsItsAddressAfterTheLabel() {
    assertEquals("Ping Jane Doe (jane@x.com) or Email us (help@x.com).",
        SymphonySyntaxNormalizer.toPlainMarkdown(
            "Ping [Jane Doe](mailto:jane@x.com) or [Email us](mailto:help@x.com)."));
  }

  @Test
  void plainMailtoLink_whoseLabelShowsTheAddress_isReducedToTheLabel() {
    assertEquals("Write to thibault.pensec@symphony.com or Jane <jane@x.com>.",
        SymphonySyntaxNormalizer.toPlainMarkdown(
            "Write to [thibault.pensec@symphony.com](mailto:thibault.pensec@symphony.com)"
                + " or [Jane <jane@x.com>](mailto:JANE@x.com)."));
  }

  @Test
  void plainMailtoLink_withAQuery_keepsTheAddressOnly() {
    assertEquals("Mail us (help@x.com)", SymphonySyntaxNormalizer.toPlainMarkdown("[Mail us](mailto:help@x.com?subject=Hi)"));
  }

  /** Bare forms are already plain text - reducing them would change the words the model wrote. */
  @Test
  void bareForms_areLeftAlone() {
    String markdown = "Filed under #compliance for $AAPL, cost $12 million.";

    assertEquals(markdown, SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void ordinaryLinks_areNotTouched() {
    String markdown = "See [the docs](https://example.com/docs).";

    assertEquals(markdown, SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void ordinaryMarkdown_survivesUnchanged() {
    String markdown = """
        # Heading

        - one
        - two

        | A | B |
        |---|---|
        | 1 | 2 |""";

    assertEquals(markdown, SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void mixedContent_hasNoDirectiveMarkersLeft() {
    String markdown = """
        Intro paragraph.

        :::expand Quarterly breakdown
        Revenue grew, see #[quarterly-review].
        :::

        Closing paragraph.""";

    String normalized = SymphonySyntaxNormalizer.toPlainMarkdown(markdown);

    assertFalse(normalized.contains(":::"), normalized);
    assertFalse(normalized.contains("#["), normalized);
    assertEquals("""
        Intro paragraph.

        **Quarterly breakdown**
        Revenue grew, see quarterly-review.

        Closing paragraph.""", normalized);
  }

  @Test
  void cardDirective_withAQuotedAttributeValueHoldingSpaces_dropsTheWholePair() {
    String markdown = """
        :::card color="dark blue" accent='light blue' Risk summary
        Body.
        :::""";

    assertEquals("**Risk summary**\nBody.", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void windowsLineEndings_areNormalizedLikeUnixOnes() {
    String markdown = "Intro.\r\n\r\n:::card\r\n\r\n\r\nBody.\r\n:::\r\n\r\n\r\nOutro.";

    assertEquals("Intro.\n\nBody.\n\nOutro.", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void windowsLineEndings_doNotBreakNativeBlockReplacement() {
    String markdown = "Intro.\r\n\r\n<messageML>\r\n<h1>Hello</h1>\r\n</messageML>\r\n\r\nOutro.";

    assertEquals("Intro.\n\n[rich content]\n\nOutro.", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void loneCarriageReturns_areTreatedAsLineBreaks() {
    assertEquals("**Detail**\nBody.",
        SymphonySyntaxNormalizer.toPlainMarkdown(":::expand Detail\rBody.\r:::"));
  }

  /** Each pass is skipped when its trigger text is absent, so a pass must still fire when only its own trigger is there. */
  @Test
  void eachPass_stillFiresWhenOnlyItsOwnTriggerIsPresent() {
    assertEquals("**T**\nB", SymphonySyntaxNormalizer.toPlainMarkdown(":::card T\nB\n:::"));
    assertEquals("See topic.", SymphonySyntaxNormalizer.toPlainMarkdown("See #[topic]."));
    assertEquals("[rich content]", SymphonySyntaxNormalizer.toPlainMarkdown("<messageML><b>x</b></messageML>"));
    assertEquals("a < b and c > d", SymphonySyntaxNormalizer.toPlainMarkdown("a < b and c > d"));
  }

  @Test
  void nullAndEmptyInputs_areSafe() {
    assertEquals("", SymphonySyntaxNormalizer.toPlainMarkdown(null));
    assertEquals("", SymphonySyntaxNormalizer.toPlainMarkdown(""));
  }

  // ---------------------------------------------------------------------------------------------
  // Native MessageML blocks
  // ---------------------------------------------------------------------------------------------

  @Test
  void nativeBlock_isReplacedWithTheRichContentMarker() {
    String markdown = """
        <messageML>
        <h1>Hello</h1>
        </messageML>""";

    assertEquals("[rich content]", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void markdownAndBlocks_mixedInOrder_eachBlockBecomesItsOwnMarker() {
    String markdown = """
        Intro paragraph.

        <messageML>
        <h1>First</h1>
        </messageML>

        Middle paragraph.

        <messageML>
        <h1>Second</h1>
        </messageML>

        Closing paragraph.""";

    assertEquals("""
        Intro paragraph.

        [rich content]

        Middle paragraph.

        [rich content]

        Closing paragraph.""", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void unterminatedNativeBlock_isStillReplaced() {
    String markdown = """
        Intro.

        <messageML>
        <h1>Unfinished""";

    assertEquals("Intro.\n\n[rich content]", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void textAfterTheClosingTag_isKeptAfterTheMarker() {
    String markdown = "Intro.\n\n<messageML><card/></messageML> follow-up text\n\nOutro.";

    assertEquals("Intro.\n\n[rich content]\nfollow-up text\n\nOutro.",
        SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  /** A regex alone can't tell a fenced example apart from a real block - the parser can. */
  @Test
  void nativeBlockInsideAFencedExample_survivesUntouched() {
    String markdown = """
        Here's the syntax:

        ```
        <messageML>
        <h1>Example</h1>
        </messageML>
        ```

        Use it like that.""";

    assertEquals(markdown, SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void nativeBlockContent_neverLeaksIntoPublishedText() {
    String markdown = """
        <messageML>
        <card><body>Secret internal markup</body></card>
        </messageML>""";

    String normalized = SymphonySyntaxNormalizer.toPlainMarkdown(markdown);

    assertFalse(normalized.contains("<card"), normalized);
    assertFalse(normalized.contains("Secret internal markup"), normalized);
  }

  // ---------------------------------------------------------------------------------------------
  // Code is kept verbatim: it shows the syntax, it does not use it
  // ---------------------------------------------------------------------------------------------

  @Test
  void fencedCode_showingTheSyntax_isKeptVerbatim() {
    String markdown = """
        Write it like this:

        ```
        :::card Title
        Ask @[Jane](mailto:jane@x.com) about #[topic] and $[AAPL]
        :::
        ```

        Then #[topic] works.""";

    String expected = markdown.replace("Then #[topic] works.", "Then topic works.");

    assertEquals(expected, SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void tildeAndLongerFences_areCodeToo() {
    String markdown = "~~~md\n#[a] $[B]\n~~~\n\n````\n```\n@[c](mailto:c@x.com)\n```\n````\n\n#[d]";

    assertEquals("~~~md\n#[a] $[B]\n~~~\n\n````\n```\n@[c](mailto:c@x.com)\n```\n````\n\nd",
        SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void unterminatedFence_staysCodeToTheEnd() {
    String markdown = "Intro #[a]\n\n```\n:::card Title\n#[b]";

    assertEquals("Intro a\n\n```\n:::card Title\n#[b]", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void indentedCode_isKeptVerbatim() {
    String markdown = "Intro\n\n    $[AAPL] and [Jane](mailto:jane@x.com)\n\nOutro $[AAPL]";

    assertEquals("Intro\n\n    $[AAPL] and [Jane](mailto:jane@x.com)\n\nOutro AAPL",
        SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void inlineCode_isKeptVerbatim() {
    String markdown = "Tag with `#[topic]` or `@[Jane](mailto:jane@x.com)`, as in #[topic].";

    assertEquals("Tag with `#[topic]` or `@[Jane](mailto:jane@x.com)`, as in topic.",
        SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void inlineCodeSpanningTwoLines_isKeptVerbatim() {
    String markdown = "Use `#[topic]\nor $[AAPL]` here and $[MSFT].";

    assertEquals("Use `#[topic]\nor $[AAPL]` here and MSFT.", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void inlineCode_inADirectiveTitle_staysInsideTheBoldLabel() {
    assertEquals("**Use `#[x]`**\nBody.", SymphonySyntaxNormalizer.toPlainMarkdown(":::card Use `#[x]`\nBody.\n:::"));
  }

  @Test
  void fencedCode_insideADirectiveBody_isKeptVerbatim() {
    String markdown = ":::card Demo\n```\n#[x] @[a](mailto:a@x.com)\n```\nAfter #[y]\n:::";

    assertEquals("**Demo**\n```\n#[x] @[a](mailto:a@x.com)\n```\nAfter y", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void codeInsideABlockQuoteOrAList_isKeptVerbatim() {
    String markdown = "> ```\n> $[AAPL]\n> ```\n\n- item `#[a]`\n- real #[b]";

    assertEquals("> ```\n> $[AAPL]\n> ```\n\n- item `#[a]`\n- real b", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void blankLinesInsideCode_areNotCollapsed() {
    String markdown = "```\na\n\n\n\nb\n```\n\n#[t]";

    assertEquals("```\na\n\n\n\nb\n```\n\nt", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void nativeBlockNextToCode_isStillReplaced() {
    String markdown = "`#[a]`\n\n<messageML>\n<p>x</p>\n</messageML>\n\n```\n#[b]\n```";

    assertEquals("`#[a]`\n\n[rich content]\n\n```\n#[b]\n```", SymphonySyntaxNormalizer.toPlainMarkdown(markdown));
  }

  @Test
  void textHoldingTheShelfCharacters_isStillNormalized() {
    assertEquals("a \uE000 b \uE001 topic", SymphonySyntaxNormalizer.toPlainMarkdown("a \uE000 b \uE001 #[topic]"));
    assertEquals("**T**\nBody\n\uE000", SymphonySyntaxNormalizer.toPlainMarkdown(":::card T\nBody\n:::\n\uE000"));
  }
}
