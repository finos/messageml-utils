package org.finos.symphony.messageml.messagemlutils.compose;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.util.NoOpDataProvider;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * The {@code :::card} / {@code :::expand} container directives.
 *
 * <p>Every test asserts validity as well as shape, because the card content model is enforced by
 * {@code assertParent} and {@code assertNoAttributes} rather than by an unknown-tag error: a card that
 * is nested wrongly or carries an attribute on its header fails at a place that is easy to miss when
 * only comparing strings.
 */
class ContainerDirectiveTest {

    private final MarkdownToMessageMLConverter converter = new MarkdownToMessageMLConverter();

    private static final MessageMLRenderOptions GROUPING = new MessageMLRenderOptions(true, true, false, false);
    private static final MessageMLRenderOptions GROUPING_STREAMING =
            new MessageMLRenderOptions(true, true, true, false);

    // ---------------------------------------------------------------------------------------------
    // Cards
    // ---------------------------------------------------------------------------------------------

    @Test
    void card_withLeadingHeading_usesItAsTheHeader() {
        String md = """
                :::card
                ### Risk summary
                Exposure is **within** limits.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><card><header>Risk summary</header>"
                + "<body>Exposure is <b>within</b> limits.</body></card></messageML>", mml);
        assertValid(mml);
    }

    @Test
    void card_withoutHeading_hasNoHeader() {
        String md = """
                :::card
                Just body content.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><card><body>Just body content.</body></card></messageML>", mml);
        assertFalse(mml.contains("<header>"));
        assertValid(mml);
    }

    @Test
    void card_withAccentAndIcon_carriesBothAttributes() {
        String md = """
                :::card accent=blue icon=https://example.com/i.png
                ### Header
                Body.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><card accent=\"blue\" iconSrc=\"https://example.com/i.png\">"
                + "<header>Header</header><body>Body.</body></card></messageML>", mml);
        assertValid(mml);
    }

    /** messageml-utils validates neither of these, so the allowlist is our policy, not the library's. */
    @Test
    void card_withUnsupportedAttribute_dropsIt() {
        String md = """
                :::card onclick=alert(1) accent=blue
                Body.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertFalse(mml.contains("onclick"), mml);
        assertTrue(mml.contains("accent=\"blue\""), mml);
        assertValid(mml);
    }

    @Test
    void card_withQuotedAttributeValues_unquotesThem() {
        String md = """
                :::card accent="blue" icon='https://example.com/i.png'
                Body.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><card accent=\"blue\" iconSrc=\"https://example.com/i.png\">"
                + "<body>Body.</body></card></messageML>", mml);
        assertValid(mml);
    }

    /** Without quote support the unknown pair's tail ({@code blue"}) would leak into the card header. */
    @Test
    void card_withQuotedUnknownAttributeHoldingSpaces_dropsTheWholePair() {
        String md = """
                :::card color="dark blue" Risk summary
                Body.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><card><header>Risk summary</header><body>Body.</body></card></messageML>", mml);
        assertValid(mml);
    }

    @Test
    void card_withRelativeIcon_dropsTheAttribute() {
        String md = """
                :::card icon=/local/i.png
                Body.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><card><body>Body.</body></card></messageML>", mml);
        assertValid(mml);
    }

    @Test
    void card_withSpaceAfterTheColons_isStillRecognised() {
        String md = """
                ::: card
                Body.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><card><body>Body.</body></card></messageML>", mml);
    }

    @Test
    void card_withTitleOnTheOpeningLine_usesItAsTheHeader() {
        String md = """
                :::card Risk summary
                Body.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><card><header>Risk summary</header><body>Body.</body></card></messageML>", mml);
        assertValid(mml);
    }

    @Test
    void card_bodyKeepsRichMarkdown() {
        String md = """
                :::card
                ### Header
                | A | B |
                |---|---|
                | 1 | 2 |
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertTrue(mml.contains("<table>"), mml);
        assertTrue(mml.contains("<body><table>"), mml);
        assertValid(mml);
    }

    /** Paragraph separation must still be right once header and body boundaries exist. */
    @Test
    void card_bodyWithTwoParagraphs_separatesThemAndDoesNotTrailABreak() {
        String md = """
                :::card
                ### Header
                First paragraph.

                Second paragraph.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><card><header>Header</header>"
                + "<body>First paragraph.<br/>Second paragraph.</body></card></messageML>", mml);
        assertValid(mml);
    }

    // ---------------------------------------------------------------------------------------------
    // Expandable cards
    // ---------------------------------------------------------------------------------------------

    @Test
    void expand_isCollapsedByDefault() {
        String md = """
                :::expand Quarterly detail
                Detailed breakdown here.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><expandable-card state=\"collapsed\">"
                + "<header>Quarterly detail</header><body>Detailed breakdown here.</body>"
                + "</expandable-card></messageML>", mml);
        assertValid(mml);
    }

    @Test
    void expand_withExpandedModifier_carriesThatState() {
        String mml = converter.renderDocument(":::expand[expanded] Detail\nBody.\n:::", GROUPING);

        assertTrue(mml.contains("state=\"expanded\""), mml);
        assertValid(mml);
    }

    @Test
    void expand_withCroppedModifier_carriesThatState() {
        String mml = converter.renderDocument(":::expand[cropped] Detail\nBody.\n:::", GROUPING);

        assertTrue(mml.contains("state=\"cropped\""), mml);
        assertValid(mml);
    }

    @Test
    void expand_withErrorModifier_marksTheBodyVariant() {
        String mml = converter.renderDocument(":::expand[error] Failure detail\nStack trace.\n:::", GROUPING);

        assertEquals("<messageML><expandable-card state=\"collapsed\"><header>Failure detail</header>"
                + "<body variant=\"error\">Stack trace.</body></expandable-card></messageML>", mml);
        assertValid(mml);
    }

    @Test
    void expand_withUnknownModifier_fallsBackToCollapsed() {
        String mml = converter.renderDocument(":::expand[sideways] Detail\nBody.\n:::", GROUPING);

        assertTrue(mml.contains("state=\"collapsed\""), mml);
        assertValid(mml);
    }

    @Test
    void expandable_longKeywordVariant_isAccepted() {
        String mml = converter.renderDocument(":::expandable Quarterly detail\nBody.\n:::", GROUPING);

        assertTrue(mml.contains("<expandable-card"), mml);
        assertTrue(mml.contains("<header>Quarterly detail</header>"), mml);
    }

    /** An empty header is a validation risk and reads as a bug to the user, so a label is substituted. */
    @Test
    void expand_withoutATitle_usesAFallbackLabel() {
        String mml = converter.renderDocument(":::expand\nBody.\n:::", GROUPING);

        assertEquals("<messageML><expandable-card state=\"collapsed\"><header>Details</header>"
                + "<body>Body.</body></expandable-card></messageML>", mml);
        assertValid(mml);
    }

    /**
     * Without this a user watching a long answer arrive sees an empty box for the whole
     * generation, because a collapsed card renders as its header alone.
     */
    @Test
    void expand_whileStreaming_isForcedExpanded() {
        String md = ":::expand[collapsed] Detail\nBody.\n:::";

        assertTrue(converter.renderDocument(md, GROUPING_STREAMING).contains("state=\"expanded\""));
        assertTrue(converter.renderDocument(md, GROUPING).contains("state=\"collapsed\""));
    }

    // ---------------------------------------------------------------------------------------------
    // Nesting, truncation and the toggle
    // ---------------------------------------------------------------------------------------------

    /**
     * Nesting is impossible by construction: the body is parsed by a parser that does not know about
     * directives, so the inner one is never a directive to begin with.
     */
    @Test
    void nestedDirective_degradesToText() {
        String md = """
                :::card
                Outer body.
                :::card
                Inner body.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals(1, countOccurrences(mml, "<card"), mml);
        assertTrue(mml.contains(":::card"), mml);
        assertValid(mml);
    }

    @Test
    void unterminatedContainer_closesAtEndOfBuffer() {
        String md = """
                :::expand Quarterly detail
                Half-written body""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><expandable-card state=\"collapsed\"><header>Quarterly detail</header>"
                + "<body>Half-written body</body></expandable-card></messageML>", mml);
        assertValid(mml);
    }

    /** With grouping off the markers are text, but the body's formatting is not thrown away. */
    @Test
    void directive_withGroupingDisabled_rendersAsLiteralTextAroundARenderedBody() {
        String md = """
                :::card
                Body with **bold**.
                :::""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>:::card<br/>Body with <b>bold</b>.<br/>:::</messageML>", mml);
        assertFalse(mml.contains("<card>"));
        assertValid(mml);
    }

    @Test
    void indentedDirective_isCodeNotAContainer() {
        String mml = converter.renderDocument("    :::card\n    Body.\n    :::", GROUPING);

        assertFalse(mml.contains("<card"), mml);
        assertTrue(mml.contains("<code>"), mml);
    }

    // ---------------------------------------------------------------------------------------------
    // Blockquote, keyed off the toggle
    // ---------------------------------------------------------------------------------------------

    @Test
    void blockquote_withGroupingDisabled_keepsTheLegacyCard() {
        String mml = converter.renderDocument("> quoted text", MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><card><body>quoted text</body></card></messageML>", mml);
        assertValid(mml);
    }

    @Test
    void blockquote_withGroupingEnabled_isEmphasisedInsteadOfACard() {
        String mml = converter.renderDocument("> quoted text", GROUPING);

        assertEquals("<messageML><i>quoted text</i></messageML>", mml);
        assertFalse(mml.contains("<card"));
        assertValid(mml);
    }

    @Test
    void withGroupingEnabled_cardsComeOnlyFromDirectives() {
        String md = """
                > quoted text

                :::card
                Card body.
                :::""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals(1, countOccurrences(mml, "<card"), mml);
        assertTrue(mml.contains("<i>quoted text</i>"), mml);
    }

    // A quote is emphasised paragraph by paragraph, never around a block element: <ul>, <ol>, <h1>-<h6>,
    // <pre> and <table> are rejected inside <i> or <b>, and the validator cannot repair that without
    // dropping the list.

    static Stream<Arguments> quotesWithBlockChildren() {
        return Stream.of(
                Arguments.of("bullet list",
                        "> Lead paragraph.\n>\n> - one\n> - two",
                        "<i>Lead paragraph.</i><br/><ul><li><i>one</i></li><li><i>two</i></li></ul>"),
                Arguments.of("ordered list",
                        "> Steps:\n>\n> 1. first\n> 2. second",
                        "<i>Steps:</i><br/><ol><li><i>first</i></li><li><i>second</i></li></ol>"),
                Arguments.of("heading",
                        "> ### Title\n> Body text.",
                        "<h3>Title</h3><i>Body text.</i>"),
                Arguments.of("fenced code block",
                        "> Run this:\n>\n> ```\n> make test\n> ```",
                        "<i>Run this:</i><br/><code>make test\n</code>"));
    }

    @ParameterizedTest(name = "quote with a {0} keeps the block outside the emphasis")
    @MethodSource("quotesWithBlockChildren")
    void blockquote_withABlockChild_keepsItOutsideTheEmphasis(String shape, String md, String expectedBody) {
        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML>" + expectedBody + "</messageML>", mml, shape);
        assertNoBlockInsideInline(mml);
        assertValid(mml);
    }

    @Test
    void blockquote_nested_isEmphasisedOnce() {
        String md = """
                > outer
                >
                > > inner""";

        String mml = converter.renderDocument(md, GROUPING);

        assertEquals("<messageML><i>outer</i><br/><i>inner</i></messageML>", mml);
        assertFalse(mml.contains("<i><i>"), mml);
        assertValid(mml);
    }

    @Test
    void blockquote_withGroupingDisabled_keepsBlockChildrenInsideTheCard() {
        String md = """
                > Lead paragraph.
                >
                > - one""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML><card><body>Lead paragraph.<br/><ul><li>one</li></ul></body></card></messageML>", mml);
        assertValid(mml);
    }

    /** A quoted reminder with a bold lead and bullets: rendered with the production options, it has to validate. */
    @Test
    void blockquote_quotedReminderWithBullets_validatesWithFullOptions() {
        String md = """
                Here is the draft I will send:

                > Please be reminded that all on-call shifts must be entered.
                >
                > **Important Time Tracking Deadline:**
                > - Active On-Call Entry Accuracy: enter exact duration.
                > - Timesheet Lock: Starting November 1st, timesheets will be locked.

                Shall I proceed?""";

        String mml = converter.renderDocument(md, MessageMLRenderOptions.full());

        assertTrue(mml.contains("<ul><li><i>Active On-Call Entry Accuracy"), mml);
        assertNoBlockInsideInline(mml);
        assertValid(mml);
    }

    // ---------------------------------------------------------------------------------------------
    // Counters
    // ---------------------------------------------------------------------------------------------

    @Test
    void render_countsCardsAndExpandables() {
        String md = """
                :::card
                One.
                :::

                :::expand Detail
                Two.
                :::""";

        RenderedMessageML rendered = converter.render(md, GROUPING);

        assertEquals(new FormCounts(0, 0, 0, 1, 1, 0, 0, 0), rendered.forms());
    }

    // ---------------------------------------------------------------------------------------------
    // Attribute-form equivalents of the bracket modifiers and lines that only look like directives
    // ---------------------------------------------------------------------------------------------

    @Test
    void expand_withStateAsAnAttribute_isAccepted() {
        String mml = converter.renderDocument(":::expand state=cropped Detail\nBody.\n:::", GROUPING);

        assertTrue(mml.contains("state=\"cropped\""), mml);
        assertTrue(mml.contains("<header>Detail</header>"), mml);
        assertValid(mml);
    }

    /** The documented bracket form wins over the attribute form, so one authored intent has one answer. */
    @Test
    void expand_withBothModifierAndStateAttribute_prefersTheModifier() {
        String mml = converter.renderDocument(":::expand[expanded] state=cropped Detail\nBody.\n:::", GROUPING);

        assertTrue(mml.contains("state=\"expanded\""), mml);
        assertValid(mml);
    }

    @Test
    void expand_withVariantAsAnAttribute_isAccepted() {
        String mml = converter.renderDocument(":::expand variant=error Detail\nBody.\n:::", GROUPING);

        assertTrue(mml.contains("<body variant=\"error\">"), mml);
        assertValid(mml);
    }

    @Test
    void expand_withUnknownVariantValue_dropsIt() {
        String mml = converter.renderDocument(":::expand variant=sideways Detail\nBody.\n:::", GROUPING);

        assertTrue(mml.contains("<body>"), mml);
        assertFalse(mml.contains("variant"), mml);
        assertValid(mml);
    }

    @Test
    void card_withUppercaseAccent_dropsIt() {
        String mml = converter.renderDocument(":::card accent=BLUE\nBody.\n:::", GROUPING);

        assertEquals("<messageML><card><body>Body.</body></card></messageML>", mml);
    }

    @Test
    void aLineThatMerelyStartsWithColons_isNotADirective() {
        String mml = converter.renderDocument("::: not a directive at all", GROUPING);

        assertFalse(mml.contains("<card"), mml);
        assertFalse(mml.contains("<expandable-card"), mml);
        assertValid(mml);
    }

    @Test
    void emptyMarkdown_rendersNothing() {
        assertEquals("<messageML></messageML>", converter.renderDocument("", GROUPING));
        assertEquals("<messageML></messageML>", converter.renderDocument(null, GROUPING));
    }

    private static int countOccurrences(String haystack, String needle) {
        int count = 0;
        int index = haystack.indexOf(needle);
        while (index >= 0) {
            count++;
            index = haystack.indexOf(needle, index + needle.length());
        }
        return count;
    }

    /** Fails when an {@code <i>} or {@code <b>} directly contains a block element. */
    private static void assertNoBlockInsideInline(String markup) {
        Pattern blockInsideInline = Pattern.compile(
                "<(i|b)>(?:(?!</\\1>).)*<(ul|ol|h[1-6]|pre|table)[\\s>]", Pattern.DOTALL);
        assertFalse(blockInsideInline.matcher(markup).find(), () -> "block element inside inline: " + markup);
    }

    private static void assertValid(String markup) {
        assertDoesNotThrow(() -> new MessageMLContext(new NoOpDataProvider()).parseMessageML(markup, null, null),
                () -> "expected valid MessageML: " + markup);
    }
}
