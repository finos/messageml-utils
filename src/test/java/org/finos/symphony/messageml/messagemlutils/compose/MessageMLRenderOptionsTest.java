package org.finos.symphony.messageml.messagemlutils.compose;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * The options record carries the streaming-versus-final distinction collapsible content
 * depends on, so the phase transitions are pinned here rather than only exercised
 * indirectly through the converter.
 */
class MessageMLRenderOptionsTest {

    @Test
    void legacy_preservesTodaysBehaviour() {
        assertFalse(MessageMLRenderOptions.LEGACY.entitySyntax());
        assertFalse(MessageMLRenderOptions.LEGACY.contentGrouping());
        assertFalse(MessageMLRenderOptions.LEGACY.intermediate());
        assertFalse(MessageMLRenderOptions.LEGACY.nativeMessageML());
    }

    @Test
    void full_enablesBothCapabilitiesForAFinalMessage() {
        MessageMLRenderOptions options = MessageMLRenderOptions.full();

        assertTrue(options.entitySyntax());
        assertTrue(options.contentGrouping());
        assertFalse(options.intermediate());
        assertFalse(options.nativeMessageML());
    }

    @Test
    void withNativeMessageML_setsTheFlagAndKeepsEverythingElse() {
        MessageMLRenderOptions options = MessageMLRenderOptions.full().withNativeMessageML(true);

        assertTrue(options.nativeMessageML());
        assertTrue(options.entitySyntax());
        assertTrue(options.contentGrouping());
        assertFalse(options.intermediate());
    }

    @Test
    void withNativeMessageML_sameValue_returnsTheSameInstance() {
        MessageMLRenderOptions options = MessageMLRenderOptions.full();

        assertSame(options, options.withNativeMessageML(false));
    }

    @Test
    void asIntermediate_flipsThePhaseAndKeepsTheCapabilities() {
        MessageMLRenderOptions intermediate =
                MessageMLRenderOptions.full().withNativeMessageML(true).asIntermediate();

        assertTrue(intermediate.intermediate());
        assertTrue(intermediate.entitySyntax());
        assertTrue(intermediate.contentGrouping());
        assertTrue(intermediate.nativeMessageML());
    }

    @Test
    void asIntermediate_onAnIntermediateValue_returnsTheSameInstance() {
        MessageMLRenderOptions intermediate = MessageMLRenderOptions.full().asIntermediate();

        assertSame(intermediate, intermediate.asIntermediate());
    }

    @Test
    void asFinal_flipsBackAndIsIdempotent() {
        MessageMLRenderOptions options = MessageMLRenderOptions.full();

        assertEquals(options, options.asIntermediate().asFinal());
        assertSame(options, options.asFinal());
    }

    @Test
    void asFinal_keepsNativeMessageML() {
        MessageMLRenderOptions intermediate =
                MessageMLRenderOptions.full().withNativeMessageML(true).asIntermediate();

        assertTrue(intermediate.asFinal().nativeMessageML());
    }

    /** Defensive: the converter is called with accumulated buffers that can be absent. */
    @Test
    void escapeHtml_nullInput_yieldsEmptyString() {
        assertEquals("", MarkdownToMessageMLConverter.escapeHtml(null));
    }
}
