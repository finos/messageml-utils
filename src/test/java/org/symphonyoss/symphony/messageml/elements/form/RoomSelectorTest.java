package org.symphonyoss.symphony.messageml.elements.form;

import static org.junit.Assert.assertEquals;
import static org.symphonyoss.symphony.messageml.markdown.MarkdownRenderer.addEscapeCharacter;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.RoomSelector;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RoomSelectorTest extends ElementTest {
  private static final String FORM_ID_ATTR = "id";

  @Test
  public void sendValidRoomSelectorOnPresentationML() throws Exception {
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR
        + "\"><div class=\"room-selector\" data-name=\"one-name\" "
        + "data-placeholder=\"some-placeholder\" data-required=\"true\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag("one-name", "some-placeholder", true);
  }

  @Test
  public void sendValidRoomSelectorWithLabelAndTitleOnPresentationML() throws Exception {
    context.parseMessageML("<messageML>"
        + "<form id=\"room-selector-element\">"
        + "<room-selector label=\"label here\" name=\"ps-name\" title=\"title here\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element roomSelector = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(RoomSelector.class, roomSelector.getClass());

    String presentationML = context.getPresentationML();
    String roomSelectorRegex = ".*(\"room-selector-(.*?)\").*";
    Pattern pattern = Pattern.compile(roomSelectorRegex);
    Matcher matcher = pattern.matcher(presentationML);
    String uniqueLabelId = matcher.matches() ? matcher.group(2) : null;

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"room-selector-element\">" +
        "<div class=\"room-selector-group\" data-generated=\"true\">" +
        "<label for=\"room-selector-" + uniqueLabelId + "\">label here</label>" +
        "<span class=\"info-hint\" data-target-id=\"room-selector-" + uniqueLabelId
        + "\" data-title=\"title here\"></span>" +
        "<div class=\"room-selector\" data-name=\"ps-name\" id=\"room-selector-" + uniqueLabelId
        + "\"></div>" +
        "</div>" +
        ACTION_BTN_ELEMENT +
        "</form>" +
        "</div>";

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, presentationML);

    String expectedMarkdownText = "[label here][title here]";
    assertEquals(
        "Form (log into desktop client to answer):\n---\n(Room Selector:" + expectedMarkdownText
            + ")" + ACTION_BTN_MARKDOWN
            + "\n---\n", context.getMarkdown());
  }

  @Test
  public void sendValidRoomSelectorWithValue() throws Exception {
    context.parseMessageML("<messageML>" +
        "  <form id=\"all-elements\">" +
        "    <room-selector name=\"room-selector\" placeholder=\"Type a room's name\" "
        + "value=\"[123]\"/>" +
        "    <button name=\"send-answers\" type=\"action\">Send Answers</button>" +
        "  </form>" +
        "</messageML>", null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();

    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\">  <form id=\"all-elements\">   "
            + " <div class=\"room-selector\" data-name=\"room-selector\" data-placeholder=\"Type "
            + "a room's name\" data-value=\"[123]\"></div>    <button type=\"action\" "
            + "name=\"send-answers\">Send Answers</button>  </form></div>";

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, presentationML);
  }

  @Test
  public void sendValidRoomSelectorWithValueMultipleIds() throws Exception {
    context.parseMessageML("<messageML>" +
        "  <form id=\"all-elements\">" +
        "    <room-selector name=\"room-selector\" placeholder=\"Type a room's name\" "
        + "value=\"[123,456]\"/>"
        +
        "    <button name=\"send-answers\" type=\"action\">Send Answers</button>" +
        "  </form>" +
        "</messageML>", null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "  <form id=\"all-elements\">" +
        "    <div class=\"room-selector\" data-name=\"room-selector\" data-placeholder=\"Type"
        + " a room's name\" data-value=\"[123,456]\"></div>"
        +
        "    <button type=\"action\" name=\"send-answers\">Send Answers</button>" +
        "  </form>" +
        "</div>";

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, presentationML);
  }

  @Test
  public void sendInvalidRoomSelectorWithValueNotALong() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "Attribute \"value\" contains an unsupported format, should be an array of user ids");
    context.parseMessageML("<messageML>" +
        "  <form id=\"all-elements\">" +
        "    <room-selector name=\"room-selector\" placeholder=\"Type a room's name\" "
        + "value=\"[123,abc]\"/>"
        +
        "    <button name=\"send-answers\" type=\"action\">Send Answers</button>" +
        "  </form>" +
        "</messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendInvalidRoomSelectorWithValueWrongFormat() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "Attribute \"value\" contains an unsupported format, should be an array of user ids");
    context.parseMessageML("<messageML>" +
        "  <form id=\"all-elements\">" +
        "    <room-selector name=\"room-selector\" placeholder=\"Type a room's name\" "
        + "value=\"xxyz\"/>"
        +
        "    <button name=\"send-answers\" type=\"action\">Send Answers</button>" +
        "  </form>" +
        "</messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendInvalidAttrRoomSelectorOnPresentationML() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"dummy\" is not allowed in \"room-selector\"");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR
            + "\"><div dummy=\"idOne\" class=\"room-selector\" "
            + "data-name=\"any-name\"/></form></messageML>",
        null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendInvalidContentRoomSelectorOnPresentationML() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "Element \"room-selector\" may not have child elements or text content");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR
            + "\"><div class=\"room-selector\" "
            + "data-name=\"any-name\"><div>hey</div></div></form></messageML>",
        null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendValidRoomSelector() throws Exception {
    String name = "some-name";
    context.parseMessageML(
        "<messageML><form id=\"" + FORM_ID_ATTR + "\"><room-selector name=\"" + name +
            "\" placeholder=\"Add some user here...\" required=\"false\"/>" + ACTION_BTN_ELEMENT
            + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag(name, "Add some user here...", false);
  }

  @Test
  public void sendValidRoomSelectorWithClosingTag() throws Exception {
    String name = "other-name";
    context.parseMessageML(
        "<messageML><form id=\"" + FORM_ID_ATTR + "\"><room-selector name=\"" + name +
            "\" placeholder=\"Add some user here...\"></room-selector>" + ACTION_BTN_ELEMENT
            + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag(name, "Add some user here...", null);
  }

  @Test
  public void sendRoomSelectorWithChildElement() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "Element \"room-selector\" may not have child elements or text content");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR
            + "\"><room-selector name=\"name\">a</room-selector></form></messageML>", null,
        MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendRoomSelectorWithInvalidRequiredAttribute() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "Attribute \"required\" of element \"room-selector\" can only be one of the following "
            + "values: [true, false]");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR
            + "\"><room-selector name=\"some-name\" placeholder=\"Room placeholder\" "
            + "required=\"invalid\"/></form></messageML>",
        null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendRoomSelectorWithInvalidAttribute() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"room-selector\"");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR
            + "\"><room-selector class=\"some-class\"/></form></messageML>", null,
        MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendRoomSelectorWithBlankName() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR
            + "\"><room-selector name=\" \"></room-selector></form></messageML>", null,
        MessageML.MESSAGEML_VERSION);
  }


  @Test
  public void sendRoomSelectorOutsideForm() throws Exception {
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR
        + "\"><div><room-selector name=\"some-name\"/></div>" + ACTION_BTN_ELEMENT
        + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element roomSelector = form.getChildren().get(0).getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(roomSelector.getClass(), RoomSelector.class);
    assertEquals(
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR +
            "\"><div><div class=\"room-selector\" data-name=\"some-name\"></div></div>"
            + ACTION_BTN_ELEMENT + "</form></div>", context.getPresentationML());
    assertEquals("Form (log into desktop client to answer):\n---\n(Room Selector)\n\n"
        + ACTION_BTN_MARKDOWN
        + "\n---\n", context.getMarkdown());
  }

  @Test
  public void testBiContextRoomSelector()
      throws InvalidInputException, IOException, ProcessingException {
    MessageMLContext messageMLContext = new MessageMLContext(null);
    String input = "<messageML>"
        + "<form id=\"form_id\">"
        + "<room-selector name=\"name01\" title=\"title01\" label=\"label01\" "
        + "placeholder=\"placeholder01\" required=\"true\"/>"
        + "<room-selector name=\"name02\" title=\"title02\" "
        + "placeholder=\"placeholder02\"/>"
        + "<button name=\"room-selector\">Submit</button>"
        + "</form>"
        + "</messageML>";

    messageMLContext.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    List<BiItem> items = messageMLContext.getBiContext().getItems();

    Map<Object, Object> expectedAttributesFirst = Stream.of(new Object[][] {
        {BiFields.PLACEHOLDER.getValue(), 1},
        {BiFields.TITLE.getValue(), 1},
        {BiFields.LABEL.getValue(), 1},
        {BiFields.REQUIRED.getValue(), 1},
    }).collect(Collectors.toMap(property -> property[0], property -> property[1]));

    Map<Object, Object> expectedAttributesSecond = Stream.of(new Object[][] {
        {BiFields.PLACEHOLDER.getValue(), 1},
        {BiFields.TITLE.getValue(), 1},
    }).collect(Collectors.toMap(property -> property[0], property -> property[1]));

    BiItem roomSelectorFirstBiItemExpected = new BiItem(BiFields.ROOM_SELECTOR.getValue(),
        expectedAttributesFirst.entrySet()
            .stream()
            .collect(Collectors.toMap(e ->
                String.valueOf(e.getKey()), Map.Entry::getValue)));

    BiItem roomSelectorSecondBiItemExpected = new BiItem(BiFields.ROOM_SELECTOR.getValue(),
        expectedAttributesSecond.entrySet()
            .stream()
            .collect(Collectors.toMap(e ->
                String.valueOf(e.getKey()), Map.Entry::getValue)));


    assertEquals(5, items.size());
    assertEquals(BiFields.ROOM_SELECTOR.getValue(), items.get(0).getName());
    assertEquals(BiFields.ROOM_SELECTOR.getValue(), items.get(1).getName());
    assertSameBiItem(roomSelectorFirstBiItemExpected, items.get(0));
    assertSameBiItem(roomSelectorSecondBiItemExpected, items.get(1));
    assertMessageLengthBiItem(items.get(4), input.length());
  }

  private void assertDataFromValidParsedTag(String dataName, String dataPlaceholder,
      Boolean dataRequired) {
    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element roomSelector = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(roomSelector.getClass(), RoomSelector.class);
    assertEquals(
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR +
            "\"><div class=\"room-selector\" data-name=\"" + dataName + "\"" +
            (dataPlaceholder != null ? " data-placeholder=\"" + dataPlaceholder + "\"" : "") +
            (dataRequired != null ? " data-required=\"" + dataRequired.toString() + "\"" : "") +
            "></div>" + ACTION_BTN_ELEMENT + "</form></div>", context.getPresentationML());
    String expectedMarkdownText =
        (dataPlaceholder != null) ? ":[" + addEscapeCharacter(dataPlaceholder) + "]" : "";
    assertEquals(
        "Form (log into desktop client to answer):\n---\n(Room Selector" + expectedMarkdownText
            + ")" + ACTION_BTN_MARKDOWN
            + "\n---\n", context.getMarkdown());
  }

}
