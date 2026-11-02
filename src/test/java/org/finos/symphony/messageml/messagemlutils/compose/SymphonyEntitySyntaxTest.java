package org.finos.symphony.messageml.messagemlutils.compose;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.util.NoOpDataProvider;
import org.junit.jupiter.api.Test;

/**
 * The extended entity syntax, now parsed as commonmark inline content rather than rewritten in rendered
 * text. The layer matters: a defect existed only because a regex ran after commonmark had consumed the
 * source it was looking for and the tests below pin the properties that come free from working on the
 * AST - verbatim content is untouched and an unrecognised form costs nothing.
 */
class SymphonyEntitySyntaxTest {

    private final MarkdownToMessageMLConverter converter = new MarkdownToMessageMLConverter();

    private static final MessageMLRenderOptions ENTITIES = new MessageMLRenderOptions(true, false, false, false);

    // ---------------------------------------------------------------------------------------------
    // Explicit bracket forms
    // ---------------------------------------------------------------------------------------------

    @Test
    void explicitHashtag_producesHashElement() {
        String mml = converter.renderDocument("Filed under #[quarterly-review] today.", ENTITIES);

        assertEquals("<messageML>Filed under <hash tag=\"quarterly-review\"/> today.</messageML>", mml);
        assertValid(mml);
    }

    @Test
    void explicitCashtag_producesCashElement() {
        String mml = converter.renderDocument("Compare $[AAPL] and $[MSFT].", ENTITIES);

        assertEquals("<messageML>Compare <cash tag=\"AAPL\"/> and <cash tag=\"MSFT\"/>.</messageML>", mml);
        assertValid(mml);
    }

    /**
     * The toggle is what makes the syntax shippable without changing any existing agent: an agent
     * that was never taught the syntax should not have a bracket form silently become a tag.
     */
    @Test
    void explicitForms_withEntitySyntaxDisabled_stayLiteralText() {
        String mml = converter.renderDocument("Filed under #[quarterly-review] for $[AAPL].",
                MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Filed under #[quarterly-review] for $[AAPL].</messageML>", mml);
        assertFalse(mml.contains("<hash"));
        assertFalse(mml.contains("<cash"));
        assertValid(mml);
    }

    @Test
    void explicitForm_withWhitespaceInside_isNotAnEntity() {
        String mml = converter.renderDocument("Filed under #[not a tag].", ENTITIES);

        assertEquals("<messageML>Filed under #[not a tag].</messageML>", mml);
        assertValid(mml);
    }

    @Test
    void explicitForm_unterminated_isNotAnEntity() {
        String mml = converter.renderDocument("Filed under #[unterminated", ENTITIES);

        assertEquals("<messageML>Filed under #[unterminated</messageML>", mml);
        assertValid(mml);
    }

    @Test
    void explicitForm_empty_isNotAnEntity() {
        String mml = converter.renderDocument("Empty #[] here.", ENTITIES);

        assertEquals("<messageML>Empty #[] here.</messageML>", mml);
        assertValid(mml);
    }

    // ---------------------------------------------------------------------------------------------
    // Bare forms: always on, because a toggle cannot stop a model from writing them
    // ---------------------------------------------------------------------------------------------

    @Test
    void bareForms_areRecognisedRegardlessOfTheToggle() {
        String withSyntax = converter.renderDocument("Filed under #compliance for $AAPL.", ENTITIES);
        String withoutSyntax = converter.renderDocument("Filed under #compliance for $AAPL.",
                MessageMLRenderOptions.LEGACY);

        assertEquals("<messageML>Filed under <hash tag=\"compliance\"/> for <cash tag=\"AAPL\"/>.</messageML>",
                withSyntax);
        assertEquals(withSyntax, withoutSyntax);
    }

    @Test
    void sigilInsideAWord_isNotAnEntity() {
        String mml = converter.renderDocument("The path a#b and cost x$USD stay plain.", ENTITIES);

        assertEquals("<messageML>The path a#b and cost x$USD stay plain.</messageML>", mml);
        assertValid(mml);
    }

    // ---------------------------------------------------------------------------------------------
    // Verbatim content - free from parsing on the AST rather than on rendered text
    // ---------------------------------------------------------------------------------------------

    @Test
    void entitySyntaxInsideInlineCode_isVerbatim() {
        String mml = converter.renderDocument("Use `#[tag]` and `$AAPL` literally.", ENTITIES);

        assertFalse(mml.contains("<hash"), mml);
        assertFalse(mml.contains("<cash"), mml);
        assertValid(mml);
    }

    @Test
    void entitySyntaxInsideFencedCode_isVerbatim() {
        String mml = converter.renderDocument("```\n#[tag] and $AAPL\n```", ENTITIES);

        assertEquals("<messageML><code>#[tag] and $AAPL\n</code></messageML>", mml);
        assertFalse(mml.contains("<hash"));
        assertValid(mml);
    }

    @Test
    void entitySyntaxInALinkDestination_isVerbatim() {
        String mml = converter.renderDocument("See [docs](https://example.com/x#[tag]).", ENTITIES);

        assertFalse(mml.contains("<hash"), mml);
        assertValid(mml);
    }

    // ---------------------------------------------------------------------------------------------
    // Form counters
    // ---------------------------------------------------------------------------------------------

    @Test
    void render_reportsTheFormsItProduced() {
        RenderedMessageML rendered = converter.render(
                "Ask @[Jane](mailto:jane@x.com) about #[topic] and $[AAPL], see also #compliance.",
                ENTITIES);

        assertEquals(new FormCounts(1, 2, 1, 0, 0, 0, 0, 0), rendered.forms());
    }

    @Test
    void render_countsNothingForOrdinaryProse() {
        RenderedMessageML rendered = converter.render("A perfectly ordinary sentence.", ENTITIES);

        assertEquals(FormCounts.NONE, rendered.forms());
        assertEquals(true, rendered.forms().isEmpty());
    }

    /** A form that degraded to text was not produced, so it is not counted. */
    @Test
    void render_doesNotCountAFormThatDegradedToText() {
        RenderedMessageML rendered = converter.render("Filed under #[topic].", MessageMLRenderOptions.LEGACY);

        assertEquals(FormCounts.NONE, rendered.forms());
    }

    /** Each field alone must make the tally non-empty or a produced form would go unreported. */
    @Test
    void formCounts_isEmptyOnlyWhenNothingWasProduced() {
        assertTrue(FormCounts.NONE.isEmpty());
        assertFalse(new FormCounts(1, 0, 0, 0, 0, 0, 0, 0).isEmpty());
        assertFalse(new FormCounts(0, 1, 0, 0, 0, 0, 0, 0).isEmpty());
        assertFalse(new FormCounts(0, 0, 1, 0, 0, 0, 0, 0).isEmpty());
        assertFalse(new FormCounts(0, 0, 0, 1, 0, 0, 0, 0).isEmpty());
        assertFalse(new FormCounts(0, 0, 0, 0, 1, 0, 0, 0).isEmpty());
        assertFalse(new FormCounts(0, 0, 0, 0, 0, 1, 0, 0).isEmpty());
        assertFalse(new FormCounts(0, 0, 0, 0, 0, 0, 1, 0).isEmpty());
        assertFalse(new FormCounts(0, 0, 0, 0, 0, 0, 0, 1).isEmpty());
    }

    @Test
    void render_ofNullMarkdown_isEmptyRatherThanAFailure() {
        RenderedMessageML rendered = converter.render(null, ENTITIES);

        assertEquals("", rendered.body());
        assertEquals(FormCounts.NONE, rendered.forms());
    }

    private static void assertValid(String markup) {
        assertDoesNotThrow(() -> new MessageMLContext(new NoOpDataProvider()).parseMessageML(markup, null, null),
                () -> "expected valid MessageML: " + markup);
    }
}
