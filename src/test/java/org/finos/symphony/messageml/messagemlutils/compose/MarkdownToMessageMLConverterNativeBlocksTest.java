package org.finos.symphony.messageml.messagemlutils.compose;

import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.util.NoOpDataProvider;
import org.junit.jupiter.api.BeforeEach;
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
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

/**
 * Wires {@link NativeMessageMLRenderer} into {@link MarkdownToMessageMLConverter}'s visitor
 * and the native outcome counters on {@link FormCounts}. Kept separate
 * from {@code MarkdownToMessageMLConverterTest} to avoid churn in that already-large file; the
 * flag-off / block-detection behaviour it covers is untouched here.
 */
class MarkdownToMessageMLConverterNativeBlocksTest {

    private MarkdownToMessageMLConverter converter;
    private MessageMLContext messageMLContext;

    private static final MessageMLRenderOptions FINAL = MessageMLRenderOptions.full().withNativeMessageML(true);
    private static final MessageMLRenderOptions INTERMEDIATE = FINAL.asIntermediate();

    @BeforeEach
    void setUp() {
        converter = new MarkdownToMessageMLConverter();
        messageMLContext = new MessageMLContext(new NoOpDataProvider());
    }

    private void assertValidMessageML(String messageML) {
        assertDoesNotThrow(() -> messageMLContext.parseMessageML(messageML, null, null),
                "Generated MessageML should be parseable: " + messageML);
        assertNotNull(messageMLContext.getMessageML());
    }

    // ---------------------------------------------------------------------------------------------
    // Mixed Markdown and native blocks, flag on, everything valid
    // ---------------------------------------------------------------------------------------------

    @Test
    void mixedAnswer_withTwoValidBlocks_producesOneValidDocumentWithBothInlined() {
        String md = """
                # PROJ-1032 status

                Here is the current state of the ticket.

                <messageML><card><header>Ticket details</header><body>Status: in progress, owner: Thibault</body></card></messageML>

                And here is what changed in this release.

                <messageML><expandable-card state="collapsed"><header>Changelog</header><body>Added the native block renderer and its tests.</body></expandable-card></messageML>

                That is everything for this update.""";

        String mml = converter.renderDocument(md, FINAL);

        assertValidMessageML(mml);
        assertTrue(mml.contains("<card><header>Ticket details</header>"
                + "<body>Status: in progress, owner: Thibault</body></card>"), mml);
        assertTrue(mml.contains("<expandable-card state=\"collapsed\"><header>Changelog</header>"
                + "<body>Added the native block renderer and its tests.</body></expandable-card>"), mml);
        assertTrue(mml.contains("<h1>PROJ-1032 status</h1>"), mml);
        assertTrue(mml.contains("That is everything for this update."), mml);
        // The blocks are inlined as real MessageML, not shown back as escaped source.
        assertFalse(mml.contains("&lt;card&gt;"), mml);
        assertFalse(mml.contains("&lt;expandable-card"), mml);
    }

    // ---------------------------------------------------------------------------------------------
    // Text on the closing line, after the block
    // ---------------------------------------------------------------------------------------------

    @Test
    void textAfterTheClosingTag_isRenderedAsMarkdownAfterTheBlock() {
        String md = "<messageML><card><body>x</body></card></messageML> Note: **see** above";

        String mml = converter.renderDocument(md, FINAL);

        assertValidMessageML(mml);
        assertEquals("<messageML><card><body>x</body></card>Note: <b>see</b> above</messageML>", mml);
    }

    @Test
    void unpaddedAndPaddedTags_renderTheSameBlock() {
        String padded = converter.renderDocument("<messageML >\n<card><body>x</body></card>\n</messageML >", FINAL);
        String plain = converter.renderDocument("<messageML>\n<card><body>x</body></card>\n</messageML>", FINAL);

        assertEquals(plain, padded);
        assertTrue(padded.contains("<card><body>x</body></card>"), padded);
    }

    // ---------------------------------------------------------------------------------------------
    // One bad block among otherwise-valid content
    // ---------------------------------------------------------------------------------------------

    @Test
    void mixedAnswer_withOneUnrepairableBlock_degradesOnlyThatBlock() {
        String md = """
                # Status

                Everything else is fine.

                <messageML><card><header>Good</header><body>This one is valid.</body></card></messageML>

                Now a broken block:

                <messageML><p>Due <dateTime value="tomorrow"/></p></messageML>

                And one more good block:

                <messageML><expandable-card state="collapsed"><header>Detail</header><body>Still fine.</body></expandable-card></messageML>

                Done.""";

        String mml = converter.renderDocument(md, FINAL);

        assertValidMessageML(mml);
        assertTrue(mml.contains("<card><header>Good</header><body>This one is valid.</body></card>"), mml);
        assertTrue(mml.contains("<expandable-card state=\"collapsed\"><header>Detail</header>"
                + "<body>Still fine.</body></expandable-card>"), mml);
        assertTrue(mml.contains("Rich content could not be displayed"), mml);
        assertTrue(mml.contains(MarkdownToMessageMLConverter.escapeHtml("<p>Due <dateTime value=\"tomorrow\"/></p>")), mml);
        assertTrue(mml.contains("Everything else is fine."), mml);
        assertTrue(mml.contains("Done."), mml);
    }

    // ---------------------------------------------------------------------------------------------
    // "C3 in group 2": every prefix of a documented table example, taken at regular offsets and run
    // through the real visitor/renderer wiring end to end, always produces a document that validates
    // as a whole - restating the balancer-level C3 at the full-pipeline level.
    // ---------------------------------------------------------------------------------------------

    @TestFactory
    Stream<DynamicTest> c3InGroup2_tablePrefixesThroughTheFullPipeline_alwaysProduceAValidDocument() {
        String fragment = readCorpusFragment("render", "tables-01.mml");
        List<DynamicTest> tests = new ArrayList<>();
        for (int cut = 16; cut < fragment.length(); cut += 16) {
            String prefix = fragment.substring(0, cut);
            tests.add(dynamicTest("prefix length " + cut, () -> assertPrefixRendersValidDocument(prefix)));
        }
        tests.add(dynamicTest("full example, unterminated",
                () -> assertPrefixRendersValidDocument(fragment)));
        tests.add(dynamicTest("full example, terminated", () -> {
            String markdown = "<messageML>" + fragment + "</messageML>";
            String mml = converter.renderDocument(markdown, INTERMEDIATE);
            assertDoesNotThrow(() -> messageMLContext.parseMessageML(mml, null, null),
                    () -> "did not validate: " + mml);
        }));
        return tests.stream();
    }

    private void assertPrefixRendersValidDocument(String prefixWithoutClosingTag) {
        String markdown = "<messageML>" + prefixWithoutClosingTag;
        String mml = assertDoesNotThrow(() -> converter.renderDocument(markdown, INTERMEDIATE),
                () -> "rendering threw for prefix: " + prefixWithoutClosingTag);
        assertDoesNotThrow(() -> messageMLContext.parseMessageML(mml, null, null),
                () -> "prefix of length " + prefixWithoutClosingTag.length()
                        + " did not render a valid document: " + mml);
    }

    // ---------------------------------------------------------------------------------------------
    // Native outcome counters
    // ---------------------------------------------------------------------------------------------

    @Test
    void nativeBlock_onIntermediateRender_reportsNoNativeCounts() {
        String md = """
                Before.

                <messageML><card><header>H</header><body>B</body></card></messageML>""";

        RenderedMessageML rendered = converter.render(md, INTERMEDIATE);

        assertEquals(0, rendered.forms().nativeRendered());
        assertEquals(0, rendered.forms().nativeRepaired());
        assertEquals(0, rendered.forms().nativeDegraded());
    }

    @Test
    void nativeBlocks_onFinalRender_countsEachOutcomeExactlyOnce() {
        String md = """
                Rendered cleanly:

                <messageML><card><header>H</header><body>B</body></card></messageML>

                Needs repair:

                <messageML><expandable-card state="collapsed" accent="blue"><header>H</header><body>B</body></expandable-card></messageML>

                Cannot be repaired:

                <messageML><p>Due <dateTime value="tomorrow"/></p></messageML>""";

        RenderedMessageML rendered = converter.render(md, FINAL);

        assertEquals(1, rendered.forms().nativeRendered(), rendered.body());
        assertEquals(1, rendered.forms().nativeRepaired(), rendered.body());
        assertEquals(1, rendered.forms().nativeDegraded(), rendered.body());
    }

    // ---------------------------------------------------------------------------------------------
    // Helpers
    // ---------------------------------------------------------------------------------------------

    private static String readCorpusFragment(String bucket, String fileName) {
        try {
            URL fileUrl = MarkdownToMessageMLConverterNativeBlocksTest.class.getClassLoader()
                    .getResource("compose/messageml-doc-corpus/" + bucket + "/" + fileName);
            assertNotNull(fileUrl, () -> "missing corpus file: " + bucket + "/" + fileName);
            Path path = Paths.get(fileUrl.toURI());
            return Files.readString(path).strip();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }
}
