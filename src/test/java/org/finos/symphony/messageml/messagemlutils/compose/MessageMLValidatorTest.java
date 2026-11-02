package org.finos.symphony.messageml.messagemlutils.compose;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.util.NoOpDataProvider;
import org.junit.jupiter.api.Test;

/**
 * Covers validation, repair and fallback for MessageML documents. Every input here is a real failure
 * shape: the two the live benchmark run produced (an attribute invented by analogy, a stray ampersand
 * in prose), the structural faults the capability spec requires the pipeline to absorb (unterminated
 * element, element outside the MessageML set, non-absolute anchor) and the entity-payload case that a
 * pipeline validating markup alone would reject.
 *
 * <p>The delivery-side half - which counter is incremented, how a BDK message is rebuilt around
 * repaired content - belongs to the caller and is not covered here.
 */
class MessageMLValidatorTest {

  private final MessageMLValidator validator = new MessageMLValidator();

  // -------------------------------------------------------------------------------------------
  // validate
  // -------------------------------------------------------------------------------------------

  @Test
  void validate_validDocument_returnsNoError() {
    assertNull(validator.validate("<messageML>Hello <b>world</b></messageML>", null));
  }

  @Test
  void validate_emptyContent_isReportedRatherThanParsed() {
    assertEquals("empty content", validator.validate("   ", null));
    assertEquals("empty content", validator.validate(null, null));
  }

  @Test
  void validate_entityDivWithoutPayload_fails() {
    String markup = "<messageML><div class=\"entity\" data-entity-id=\"aiData\"/>Body</messageML>";

    String error = validator.validate(markup, null);

    assertNotNull(error);
    assertTrue(error.contains("no entity data provided"), error);
  }

  /**
   * The hard constraint: the payload that will be sent must be handed to the validator with the
   * markup or every message carrying an entity payload is rejected.
   */
  @Test
  void validate_entityDivWithItsPayload_passes() {
    String markup = "<messageML><div class=\"entity\" data-entity-id=\"aiData\"/>Body</messageML>";
    String entityJson = "{\"aiData\":{\"type\":\"com.symphony.ai.agent.response\",\"version\":\"0.1\"}}";

    assertNull(validator.validate(markup, entityJson));
  }

  // -------------------------------------------------------------------------------------------
  // repair
  // -------------------------------------------------------------------------------------------

  @Test
  void repair_unterminatedElement_isClosed() {
    String markup = "<messageML>Hello <b>world</messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML>Hello <b>world</b></messageML>", repaired);
    assertValid(repaired);
  }

  /** Closing one element exposes the next one out, which is why repair iterates. */
  @Test
  void repair_nestedUnterminatedElements_areAllClosed() {
    String markup = "<messageML>Items <ul><li>one</messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML>Items <ul><li>one</li></ul></messageML>", repaired);
    assertValid(repaired);
  }

  @Test
  void repair_disallowedElement_isUnwrappedKeepingItsText() {
    String markup = "<messageML>Hello <marquee>world</marquee></messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML>Hello world</messageML>", repaired);
    assertValid(repaired);
  }

  /** The first failure mode the live run produced: an attribute invented by analogy with {@code <card>}. */
  @Test
  void repair_attributeNotAllowedOnItsElement_isDropped() {
    String markup = "<messageML><expandable-card state=\"collapsed\" accent=\"blue\">"
        + "<header>H</header><body>B</body></expandable-card></messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML><expandable-card state=\"collapsed\">"
        + "<header>H</header><body>B</body></expandable-card></messageML>", repaired);
    assertValid(repaired);
  }

  /** The second failure mode the live run produced: an unescaped ampersand in the prose "P&L". */
  @Test
  void repair_blockElementInsideAnElementThatOnlyTakesPhrasing_isUnwrapped() {
    String markup = "<messageML><b><div>x</div></b></messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML><b>x</b></messageML>", repaired);
    assertValid(repaired);
  }

  @Test
  void repair_strayAmpersand_isEscaped() {
    String markup = "<messageML>P&L was up</messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML>P&amp;L was up</messageML>", repaired);
    assertValid(repaired);
  }

  @Test
  void repair_strayAmpersand_leavesRealEntityReferencesAlone() {
    String markup = "<messageML>P&L &amp; more</messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML>P&amp;L &amp; more</messageML>", repaired);
    assertValid(repaired);
  }

  /** Non-absolute anchors at the pipeline level, for markup the converter did not author. */
  @Test
  void repair_nonAbsoluteAnchor_isUnwrapped() {
    String markup = "<messageML>See <a href=\"/internal/runbook\">the runbook</a>.</messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML>See the runbook.</messageML>", repaired);
    assertValid(repaired);
  }

  @Test
  void repair_nonAbsoluteAnchor_keepsTheAbsoluteOnesInTheSameDocument() {
    String markup = "<messageML>See <a href=\"https://example.com\">docs</a> and "
        + "<a href=\"/rel\">this</a>.</messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML>See <a href=\"https://example.com\">docs</a> and this.</messageML>", repaired);
    assertValid(repaired);
  }

  /** {@code state} is the one required attribute the grouping elements have. */
  @Test
  void repair_expandableCardMissingItsState_getsTheAuthoringDefault() {
    String markup = "<messageML><expandable-card><header>H</header><body>B</body></expandable-card></messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML><expandable-card state=\"collapsed\"><header>H</header><body>B</body>"
        + "</expandable-card></messageML>", repaired);
    assertValid(repaired);
  }

  /** The validator enumerates the permitted values, so the repair does not have to guess. */
  @Test
  void repair_stateOutsideThePermittedSet_isReplacedWithAPermittedOne() {
    String markup = "<messageML><expandable-card state=\"sideways\"><header>H</header><body>B</body>"
        + "</expandable-card></messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML><expandable-card state=\"collapsed\"><header>H</header><body>B</body>"
        + "</expandable-card></messageML>", repaired);
    assertValid(repaired);
  }

  /**
   * An orphaned {@code <header>} is a grouping tag and those four tags are only legal together, so it
   * is unwrapped as grouping rather than tag by tag.
   */
  @Test
  void repair_orphanedGroupingTag_isUnwrapped() {
    String markup = "<messageML><header>orphan</header> and prose</messageML>";

    String repaired = validator.repair(markup, null, validator.validate(markup, null));

    assertEquals("<messageML>orphan and prose</messageML>", repaired);
    assertValid(repaired);
  }

  /**
   * Grouping is a presentation choice; the words inside it are the answer. A card that lands where cards
   * may not go is unwrapped whole - stripping only the {@code <card>} tags would orphan its
   * {@code <header>} and {@code <body>}, which are legal nowhere else, so the next pass would fail on
   * those instead.
   */
  @Test
  void repair_misplacedCard_isUnwrappedKeepingHeaderAndBody() {
    String markup = "<messageML><card accent=\"blue\"><header>Risk summary</header>"
        + "<body>Exposure is fine.</body></card></messageML>";

    String repaired = validator.repair(markup, null,
        "InvalidInputException: Element \"card\" can only be a child of the following elements: [messageML]");

    assertEquals("<messageML><b>Risk summary</b>Exposure is fine.</messageML>", repaired);
    assertValid(repaired);
  }

  @Test
  void repair_misplacedExpandableCard_isUnwrappedToo() {
    String markup = "<messageML><expandable-card state=\"collapsed\"><header>Detail</header>"
        + "<body variant=\"error\">Stack trace.</body></expandable-card></messageML>";

    String repaired = validator.repair(markup, null,
        "InvalidInputException: Element \"expandable-card\" can only be a child of the following elements: [messageML]");

    assertEquals("<messageML><b>Detail</b>Stack trace.</messageML>", repaired);
    assertValid(repaired);
  }

  @Test
  void repair_noError_returnsNull() {
    assertNull(validator.repair("<messageML>Hello</messageML>", null, null));
  }

  @Test
  void repair_nullMarkup_returnsNull() {
    assertNull(validator.repair(null, null, "some error"));
  }

  /** A rootless body has no {@code </messageML>} to insert before, so the end tag goes at the end. */
  @Test
  void repair_unterminatedElementInARootlessBody_appendsTheEndTag() {
    String repaired = validator.repair("Hello <b>world", null,
        "Invalid messageML: The element type \"b\" must be terminated by the matching end-tag \"</b>\".");

    assertEquals("Hello <b>world</b>", repaired);
    assertValid(MessageMLValidator.ensureRoot(repaired));
  }

  @Test
  void repair_nothingApplies_returnsNull() {
    // A missing entity payload is a defect in the outbound message, not something to rewrite blindly.
    String markup = "<messageML><div class=\"entity\" data-entity-id=\"aiData\"/>Body</messageML>";

    assertNull(validator.repair(markup, null, validator.validate(markup, null)));
  }

  // -------------------------------------------------------------------------------------------
  // fallback
  // -------------------------------------------------------------------------------------------

  @Test
  void escapedFallback_escapesTheAnswerAndTheFreeMarkerSigils() {
    String fallback = validator.escapedFallback("<messageML>whatever</messageML>",
        "Body <of> the answer, ${7*7} and #{x}");

    assertEquals("<messageML>Body &lt;of&gt; the answer, &#36;{7*7} and &#35;{x}</messageML>", fallback);
    assertValid(fallback);
  }

  /**
   * A fallback degrades the presentation, never the data: dropping the entity div would orphan the
   * payload and break the rich clients that render from it.
   */
  @Test
  void escapedFallback_preservesTheEntityDiv() {
    String fallback = validator.escapedFallback(
        "<messageML><div class=\"entity\" data-entity-id=\"aiData\"/><b>Body</b></messageML>", "Body");

    assertEquals("<messageML><div class=\"entity\" data-entity-id=\"aiData\"/>Body</messageML>", fallback);
  }

  @Test
  void escapedFallback_nullInputs_yieldAnEmptyDocument() {
    assertEquals("<messageML></messageML>", validator.escapedFallback(null, null));
  }

  // -------------------------------------------------------------------------------------------
  // bucketing and root handling
  // -------------------------------------------------------------------------------------------

  @Test
  void reasonOf_bucketsTheFailureClassesTheSpecDistinguishes() {
    assertEquals(MessageMLValidator.REASON_MISSING_ENTITY_DATA,
        MessageMLValidator.reasonOf("Error processing EntityJSON: no entity data provided for \"data-entity-id\""));
    assertEquals(MessageMLValidator.REASON_INVALID_ATTRIBUTE_VALUE,
        MessageMLValidator.reasonOf("Attribute \"accent\" is not allowed in \"expandable-card\""));
    assertEquals(MessageMLValidator.REASON_INVALID_ATTRIBUTE_VALUE,
        MessageMLValidator.reasonOf("The attribute \"href\" must contain an absolute URI"));
    assertEquals(MessageMLValidator.REASON_DISALLOWED_ELEMENT,
        MessageMLValidator.reasonOf("Invalid MessageML content at element \"marquee\""));
    assertEquals(MessageMLValidator.REASON_MALFORMED_MARKUP,
        MessageMLValidator.reasonOf("Invalid messageML: The element type \"b\" must be terminated"));
    assertEquals(MessageMLValidator.REASON_OTHER, MessageMLValidator.reasonOf("empty content"));
  }

  @Test
  void ensureRoot_wrapsOnlyARootlessBody() {
    assertEquals("<messageML>body</messageML>", MessageMLValidator.ensureRoot("body"));
    assertEquals("<messageML>body</messageML>", MessageMLValidator.ensureRoot("<messageML>body</messageML>"));
  }

  private static void assertValid(String markup) {
    assertDoesNotThrow(() -> new MessageMLContext(new NoOpDataProvider()).parseMessageML(markup, null, null),
        () -> "expected valid MessageML: " + markup);
  }
}
