package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.TestDataProvider;

public class DialogTest {

  private static final String TEXT_FIELD_FORM =
      "<form><text-field name=\"name1\" id=\"id1\" placeholder=\"placeholder1\" required=\"true\" /></form>";
  private static final String SIMPLE_DIALOG = "<dialog id=\"toto\"><title>e</title><body>f</body></dialog>";

  private final IDataProvider dataProvider = new TestDataProvider();
  private MessageMLContext context;

  @Before
  public void setUp() {
    context = new MessageMLContext(dataProvider);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogNoId() throws Exception {
    String messageML =
        "<messageML><dialog><title>title</title><body>body</body><footer>footer</footer></dialog></messageML>";
    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogEmptyId() throws Exception {
    final String messageML = buildDialogMML("");
    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogIdWithSpace() throws Exception {
    final String messageML = buildDialogMML("my id");
    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testTooLongDialogId() throws Exception {
    String dialogId = StringUtils.repeat('a', Dialog.ID_MAX_LENGTH + 1);
    final String messageML = buildDialogMML(dialogId);
    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithExtraAttribute() throws Exception {
    final String messageML = "<messageML><dialog id=\"dialog-id\" attribute=\"value\"><title>my title</title>"
        + "<body>my body</body></dialog></messageML>";
    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogInvalidWidth() throws Exception {
    String messageML = "<messageML><dialog id=\"dialog-id\" width=\"invalid\"/></messageML>";

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDialogValidWidth() throws Exception {
    for (String width : Dialog.ALLOWED_WIDTH_VALUES) {
      final String dialogId = "dialog-id";
      final String title = "my title";
      final String body = "my body";
      final String footer = "my footer";

      final String messageML = buildDialogMML(dialogId, width, title, body, footer);
      context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);

      assertDialogBuilt(context.getMessageML(), dialogId, width, Dialog.FALSE_STATE, title, body, footer);
    }
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogInvalidState() throws Exception {
    String messageML = "<messageML><dialog id=\"dialog-id\" state=\"invalid\"/></messageML>";

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDialogValidState() throws Exception {
    for (String state : Dialog.ALLOWED_STATE_VALUES) {
      final String dialogId = "dialog-id";
      final String width = Dialog.MEDIUM_WIDTH;
      final String title = "my title";
      final String body = "my body";
      final String footer = "my footer";

      final String messageML = buildDialogMML(dialogId, width, state, title, body, footer);
      context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);

      assertDialogBuilt(context.getMessageML(), dialogId, width, state, title, body, footer);
    }
  }

  @Test
  public void testValidDialogWithFooter() throws Exception {
    final String dialogId = "dialog-id";
    final String title = "my title";
    final String body = "my body";
    final String footer = "my footer";

    final String messageML = buildDialogMML(dialogId, title, body, footer);
    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);

    assertDialogBuilt(context.getMessageML(), dialogId, Dialog.MEDIUM_WIDTH, Dialog.FALSE_STATE, title, body, footer);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithMissingTitle() throws Exception {
    String messageML =
        "<messageML><dialog id=\"dialog-id\"><body>my body</body><footer>my footer</footer></dialog></messageML>";
    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithMissingBody() throws Exception {
    String messageML =
        "<messageML><dialog id=\"dialog-id\"><title>my title</title><footer>my footer</footer></dialog></messageML>";
    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDialogWithNoFooter() throws Exception {
    final String dialogId = "dialog-id";
    final String title = "my title";
    final String body = "my body";
    String messageMlInput =
        "<messageML><dialog id=\"" + dialogId + "\"><title>" + title + "</title><body>" + body
            + "</body></dialog></messageML>";
    context.parseMessageML(messageMlInput, null, MessageML.MESSAGEML_VERSION);

    final MessageML messageML = context.getMessageML();

    assertEquals(1, messageML.getChildren().size());
    final Element dialog = messageML.getChild(0);
    assertTrue(dialog instanceof Dialog);
    assertEquals(dialogId, dialog.getAttribute(Element.ID_ATTR));
    assertEquals(Dialog.MEDIUM_WIDTH, dialog.getAttribute(Dialog.WIDTH_ATTR));

    assertEquals(2, dialog.getChildren().size());
    assertTextNodeChild(dialog.getChild(0), DialogChild.Title.class, title);
    assertTextNodeChild(dialog.getChild(1), DialogChild.Body.class, body);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithExtraTag() throws Exception {
    String messageML = "<messageML><dialog id=\"dialog-id\"><title>my title</title><body>my body</body>"
        + "<h2>An invalid tag</h2></dialog></messageML>";

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithExtraAttributeInTitle() throws Exception {
    String messageML = "<messageML><dialog id=\"dialog-id\"><title attribute=\"value\">my title</title>"
        + "<body>my body</body></dialog></messageML>";

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithExtraAttributeInBody() throws Exception {
    String messageML = "<messageML><dialog id=\"dialog-id\"><title>my title</title>"
        + "<body attribute=\"value\">my body</body></dialog></messageML>";

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithExtraAttributeInFooter() throws Exception {
    String messageML = "<messageML><dialog id=\"dialog-id\"><title>my title</title><body>my body</body>"
        + "<footer attribute=\"value\">my footer</footer></dialog></messageML>";

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithInteractiveElementInTitle() throws Exception {
    String messageML = buildDialogMML("dialog-id", "<button>A button</button>", "body", "footer");

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithDialogInTitle() throws Exception {
    String messageML = buildDialogMML("dialog-id", SIMPLE_DIALOG, "body", "footer");

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithDialogInBody() throws Exception {
    String messageML = buildDialogMML("dialog-id", "title", SIMPLE_DIALOG, "footer");

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithDialogInFooter() throws Exception {
    String messageML = buildDialogMML("dialog-id", "title", "body", SIMPLE_DIALOG);

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithFormInTitle() throws Exception {
    String messageML = buildDialogMML("dialog-id", TEXT_FIELD_FORM, "body", "footer");

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithFormInBody() throws Exception {
    String messageML = buildDialogMML("dialog-id", "title", TEXT_FIELD_FORM, "footer");

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithFormInFooter() throws Exception {
    String messageML = buildDialogMML("dialog-id", "title", "body", TEXT_FIELD_FORM);

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithInteractiveElementInBody() throws Exception {
    String messageML = buildDialogMML("dialog-id", "title", "<button>A button</button>", "footer");

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogWithInteractiveElementInFooter() throws Exception {
    String messageML = buildDialogMML("dialog-id", "title", "body", "<button>A button</button>");

    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDialogWithNonInteractiveElementInTitle() throws Exception {
    String messageMlInput = buildDialogMML("dialog-id", "<card>A card</card>", "body", "footer");

    context.parseMessageML(messageMlInput, null, MessageML.MESSAGEML_VERSION);
    final MessageML messageML = context.getMessageML();

    final Element title = messageML.getChild(0).getChild(0);
    final Element card = title.getChild(0);
    assertEquals(Card.class, card.getClass());
  }

  @Test
  public void testDialogWithNonInteractiveElementInBody() throws Exception {
    String messageMlInput = buildDialogMML("dialog-id", "title", "<card>A card</card>", "footer");

    context.parseMessageML(messageMlInput, null, MessageML.MESSAGEML_VERSION);
    final MessageML messageML = context.getMessageML();

    final Element body = messageML.getChild(0).getChild(1);
    final Element card = body.getChild(0);
    assertEquals(Card.class, card.getClass());
  }

  @Test
  public void testDialogWithNonInteractiveElementInFooter() throws Exception {
    String messageMlInput = buildDialogMML("dialog-id", "title", "body", "<card>A card</card>");

    context.parseMessageML(messageMlInput, null, MessageML.MESSAGEML_VERSION);
    final MessageML messageML = context.getMessageML();

    final Element footer = messageML.getChild(0).getChild(2);
    final Element card = footer.getChild(0);
    assertEquals(Card.class, card.getClass());
  }

  @Test
  public void testPresentationMlConversion() throws Exception {
    String messageMlInput =
        buildDialogMML("dialog-id", Dialog.MEDIUM_WIDTH, Dialog.FALSE_STATE, "title", "body", "footer");
    context.parseMessageML(messageMlInput, null, MessageML.MESSAGEML_VERSION);

    final String expectedPattern = "^<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<dialog data-width=\"medium\" data-state=\"false\" id=\"\\S+-dialog-id\" open>"
        + "<div class=\"dialog-title\">title</div>"
        + "<div class=\"dialog-body\">body</div>"
        + "<div class=\"dialog-footer\">footer</div>"
        + "</dialog></div>$";

    final String presentationML = context.getPresentationML();
    assertTrue(presentationML.matches(expectedPattern));
  }

  @Test
  public void testMarkdownConversion() throws Exception {
    String messageMlInput =
        buildDialogMML("dialog-id", Dialog.MEDIUM_WIDTH, Dialog.FALSE_STATE, "title", "body", "footer");
    context.parseMessageML(messageMlInput, null, MessageML.MESSAGEML_VERSION);

    assertEquals("---\n"
            + "**Dialog**\n"
            + "title\n"
            + "body\n"
            + "footer\n"
            + "---\n",
        context.getMarkdown());
  }

  @Test
  public void testMarkdownConversionWithoutFooter() throws Exception {
    String messageMlInput = "<messageML>"
        + "<dialog id=\"dialog-id\">"
        + "<title><h2>A title</h2></title>"
        + "<body>body</body>"
        + "</dialog>"
        + "</messageML>";
    context.parseMessageML(messageMlInput, null, MessageML.MESSAGEML_VERSION);

    assertEquals("---\n"
            + "**Dialog**\n"
            + "**A title**\n"
            + "body\n"
            + "---\n",
        context.getMarkdown());
  }

  private void assertDialogBuilt(MessageML messageML, String dialogId, String width, String state, String title,
      String body, String footer) {
    assertEquals(1, messageML.getChildren().size());
    final Element dialog = messageML.getChild(0);
    assertTrue(dialog instanceof Dialog);
    assertEquals(dialogId, dialog.getAttribute(Element.ID_ATTR));
    assertEquals(width, dialog.getAttribute(Dialog.WIDTH_ATTR));
    assertEquals(state, dialog.getAttribute(Dialog.STATE_ATTR));

    assertEquals(3, dialog.getChildren().size());
    assertTextNodeChild(dialog.getChild(0), DialogChild.Title.class, title);
    assertTextNodeChild(dialog.getChild(1), DialogChild.Body.class, body);
    assertTextNodeChild(dialog.getChild(2), DialogChild.Footer.class, footer);
  }

  private void assertTextNodeChild(Element actualChild, Class<? extends Element> expectedType, String expectedText) {
    assertEquals(expectedType, actualChild.getClass());
    assertEquals(1, actualChild.getChildren().size());
    assertEquals(expectedText, actualChild.getChildren().get(0).asText());
  }

  private String buildDialogMML(String dialogId) {
    return "<messageML>"
        + "<dialog id=\"" + dialogId + "\">"
        + "<title>title</title>"
        + "<body>body</body>"
        + "<footer>footer</footer>"
        + "</dialog>"
        + "</messageML>";
  }

  private String buildDialogMML(String dialogId, String title, String body, String footer) {
    return "<messageML>"
        + "<dialog id=\"" + dialogId + "\">"
        + "<title>" + title + "</title>"
        + "<body>" + body + "</body>"
        + "<footer>" + footer + "</footer>"
        + "</dialog>"
        + "</messageML>";
  }

  private String buildDialogMML(String dialogId, String width, String title, String body, String footer) {
    return "<messageML>"
        + "<dialog id=\"" + dialogId + "\" width=\"" + width + "\">"
        + "<title>" + title + "</title>"
        + "<body>" + body + "</body>"
        + "<footer>" + footer + "</footer>"
        + "</dialog>"
        + "</messageML>";
  }

  private String buildDialogMML(String dialogId, String width, String state, String title, String body, String footer) {
    return "<messageML>"
        + "<dialog id=\"" + dialogId + "\" width=\"" + width + "\" state=\"" + state + "\">"
        + "<title>" + title + "</title>"
        + "<body>" + body + "</body>"
        + "<footer>" + footer + "</footer>"
        + "</dialog>"
        + "</messageML>";
  }
}
