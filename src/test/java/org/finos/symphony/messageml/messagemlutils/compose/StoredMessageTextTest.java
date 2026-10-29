package org.finos.symphony.messageml.messagemlutils.compose;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.util.NoOpDataProvider;
import org.junit.jupiter.api.Test;

/**
 * A stored card must not fuse its header into its body when the message re-enters a prompt.
 *
 * <p>The tests derive the PresentationML from messageml-utils rather than hard-coding it, so a change to
 * the library's card markup surfaces here instead of silently disabling the separation.
 */
class StoredMessageTextTest {

  private static final String CARD =
      "<messageML><card><header>Risk summary</header><body>Exposure is fine.</body></card></messageML>";
  private static final String EXPANDABLE = "<messageML><expandable-card state=\"collapsed\">"
      + "<header>Detail</header><body>Body text.</body></expandable-card></messageML>";

  /** The defect, stated as a fact about the library before the fix is applied. */
  @Test
  void withoutSeparation_headerAndBodyFuseIntoOneRun() throws Exception {
    assertEquals("Risk summaryExposure is fine.", textOf(presentationML(CARD)));
  }

  @Test
  void cardHeaderAndBody_areSeparated() throws Exception {
    String separated = StoredMessageText.separateGroupedContent(presentationML(CARD));

    assertEquals("Risk summary\nExposure is fine.", textOf(separated).strip());
  }

  @Test
  void expandableCardHeaderAndBody_areSeparated() throws Exception {
    String separated = StoredMessageText.separateGroupedContent(presentationML(EXPANDABLE));

    assertEquals("Detail\nBody text.", textOf(separated).strip());
  }

  /**
   * The alternative considered. It separates correctly, so it would have worked - pinned
   * here so the reason it was not chosen stays a cost argument (a full parse per stored message,
   * many per prompt build and a dependency on each one still being valid) rather than drifting into
   * a claim about correctness.
   */
  @Test
  void theLibrarysMarkdownRendering_alsoSeparatesButCostsAFullParse() throws Exception {
    MessageMLContext context = new MessageMLContext(new NoOpDataProvider());
    context.parseMessageML(CARD, null, null);

    assertEquals("Risk summary\n\nExposure is fine.", context.getMarkdown().strip());
  }

  /** Every message from an agent without content grouping, which must cost nothing and change nothing. */
  @Test
  void markupWithNoGrouping_isReturnedUnchanged() {
    String presentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello <b>world</b></div>";

    assertSame(presentationML, StoredMessageText.separateGroupedContent(presentationML));
  }

  @Test
  void entityDivs_areNotSeparated() {
    String presentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<div class=\"entity\" data-entity-id=\"aiData\"></div>Body</div>";

    assertSame(presentationML, StoredMessageText.separateGroupedContent(presentationML));
  }

  @Test
  void nullAndEmptyInputs_areSafe() {
    assertEquals(null, StoredMessageText.separateGroupedContent(null));
    assertEquals("", StoredMessageText.separateGroupedContent(""));
  }

  @Test
  void separation_doesNotChangeWhichWordsSurvive() throws Exception {
    String plain = textOf(presentationML(CARD)).replace("\n", "");
    String separated = textOf(StoredMessageText.separateGroupedContent(presentationML(CARD)))
        .replace("\n", "");

    assertEquals(plain, separated);
    assertTrue(separated.contains("Exposure is fine."));
  }

  private static String presentationML(String messageML) throws Exception {
    MessageMLContext context = new MessageMLContext(new NoOpDataProvider());
    context.parseMessageML(messageML, null, null);
    return context.getPresentationML();
  }

  /** Stands in for {@code PresentationMLParser.getTextContent}, which the BDK owns. */
  private static String textOf(String presentationML) {
    return presentationML.replaceAll("<[^>]+>", "");
  }
}
