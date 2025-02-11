package org.finos.symphony.messageml.messagemlutils.elements.form;

import org.junit.Test;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.elements.Element;
import org.finos.symphony.messageml.messagemlutils.elements.ElementTest;
import org.finos.symphony.messageml.messagemlutils.elements.Form;
import org.finos.symphony.messageml.messagemlutils.elements.MessageML;
import org.finos.symphony.messageml.messagemlutils.elements.PersonSelector;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.exceptions.ProcessingException;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.Assert.assertEquals;
import static org.finos.symphony.messageml.messagemlutils.markdown.MarkdownRenderer.addEscapeCharacter;

public class PersonSelectorTest extends ElementTest {
  private static final String FORM_ID_ATTR = "id";

  @Test
  public void sendValidPersonSelectorOnPresentationML() throws Exception {
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><div class=\"person-selector\" data-name=\"one-name\" data-placeholder=\"some-placeholder\" data-required=\"true\"/>" + ACTION_BTN_ELEMENT
        + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag("one-name", "some-placeholder", true);
  }

  @Test
  public void sendValidPersonSelectorWithLabelAndTitleOnPresentationML() throws Exception {
    context.parseMessageML("<messageML>"
        + "<form id=\"person-selector-element\">"
        + "<person-selector label=\"label here\" name=\"ps-name\" title=\"title here\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element personSelector = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(PersonSelector.class, personSelector.getClass());

    String presentationML = context.getPresentationML();
    String personSelectorRegex = ".*(\"person-selector-(.*?)\").*";
    Pattern pattern = Pattern.compile(personSelectorRegex);
    Matcher matcher = pattern.matcher(presentationML);
    String uniqueLabelId = matcher.matches() ? matcher.group(2) : null;
    
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"person-selector-element\">" +
        "<div class=\"person-selector-group\" data-generated=\"true\">" +
        "<label for=\"person-selector-" + uniqueLabelId + "\">label here</label>" +
        "<span class=\"info-hint\" data-target-id=\"person-selector-" + uniqueLabelId + "\" data-title=\"title here\"></span>" +
        "<div class=\"person-selector\" data-name=\"ps-name\" id=\"person-selector-" + uniqueLabelId + "\"></div>" +
        "</div>" +
        ACTION_BTN_ELEMENT +
        "</form>" +
        "</div>";

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, presentationML);

    String expectedMarkdownText = "[label here][title here]";
    assertEquals("\n   \n(Person Selector:" + expectedMarkdownText + ")" + ACTION_BTN_MARKDOWN
        + "\n   \n", context.getMarkdown());
  }

  @Test
  public void sendValidPersonSelectorWithValue() throws Exception {
    context.parseMessageML("<messageML>" +
            "  <form id=\"all-elements\">" +
            "    <person-selector name=\"person-selector\" placeholder=\"Type a person's name\" value=\"[123]\"/>" +
            "    <button name=\"send-answers\" type=\"action\">Send Answers</button>" +
            "  </form>" +
            "</messageML>", null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "  <form id=\"all-elements\">" +
            "    <div class=\"person-selector\" data-name=\"person-selector\" data-placeholder=\"Type a person's name\" data-value=\"[123]\"></div>" +
            "    <button type=\"action\" name=\"send-answers\">Send Answers</button>" +
            "  </form>" +
            "</div>";

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
            expectedPresentationML, presentationML);
  }


  @Test
  public void sendValidPersonSelectorWithFormnovalidate() throws Exception {
    context.parseMessageML("<messageML>" +
        "  <form id=\"all-elements\">" +
        "    <person-selector name=\"person-selector\" placeholder=\"Type a person's name\" formnovalidate=\"true\"/>" +
        "    <button name=\"send-answers\" type=\"action\">Send Answers</button>" +
        "  </form>" +
        "</messageML>", null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "  <form id=\"all-elements\">" +
        "    <div class=\"person-selector\" data-name=\"person-selector\" data-placeholder=\"Type a person's name\" data-formnovalidate=\"true\"></div>" +
        "    <button type=\"action\" name=\"send-answers\">Send Answers</button>" +
        "  </form>" +
        "</div>";

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, presentationML);
  }

  @Test
  public void sendValidPersonSelectorWithValueMultipleIds() throws Exception {
    context.parseMessageML("<messageML>" +
            "  <form id=\"all-elements\">" +
            "    <person-selector name=\"person-selector\" placeholder=\"Type a person's name\" value=\"[123,456]\"/>" +
            "    <button name=\"send-answers\" type=\"action\">Send Answers</button>" +
            "  </form>" +
            "</messageML>", null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "  <form id=\"all-elements\">" +
            "    <div class=\"person-selector\" data-name=\"person-selector\" data-placeholder=\"Type a person's name\" data-value=\"[123,456]\"></div>" +
            "    <button type=\"action\" name=\"send-answers\">Send Answers</button>" +
            "  </form>" +
            "</div>";

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
            expectedPresentationML, presentationML);
  }

  @Test
  public void sendInvalidPersonSelectorWithValueNotALong() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"value\" contains an unsupported format, should be an array of user ids");
    context.parseMessageML("<messageML>" +
            "  <form id=\"all-elements\">" +
            "    <person-selector name=\"person-selector\" placeholder=\"Type a person's name\" value=\"[123,abc]\"/>" +
            "    <button name=\"send-answers\" type=\"action\">Send Answers</button>" +
            "  </form>" +
            "</messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendInvalidPersonSelectorWithValueWrongFormat() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"value\" contains an unsupported format, should be an array of user ids");
    context.parseMessageML("<messageML>" +
            "  <form id=\"all-elements\">" +
            "    <person-selector name=\"person-selector\" placeholder=\"Type a person's name\" value=\"xxyz\"/>" +
            "    <button name=\"send-answers\" type=\"action\">Send Answers</button>" +
            "  </form>" +
            "</messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendInvalidAttrPersonSelectorOnPresentationML() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"dummy\" is not allowed in \"person-selector\"");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><div dummy=\"idOne\" class=\"person-selector\" data-name=\"any-name\"/></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendInvalidContentPersonSelectorOnPresentationML() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"person-selector\" may not have child elements or text content");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><div class=\"person-selector\" data-name=\"any-name\"><div>hey</div></div></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendValidPersonSelector() throws Exception {
    String name = "some-name";
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><person-selector name=\"" + name +
        "\" placeholder=\"Add some user here...\" required=\"false\"/>" + ACTION_BTN_ELEMENT
        + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag(name, "Add some user here...", false);
  }

  @Test
  public void sendValidPersonSelectorWithClosingTag() throws Exception {
    String name = "other-name";
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><person-selector name=\"" + name +
        "\" placeholder=\"Add some user here...\"></person-selector>" + ACTION_BTN_ELEMENT
        + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag(name, "Add some user here...", null);
  }

  @Test
  public void sendPersonSelectorWithChildElement() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"person-selector\" may not have child elements or text content");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><person-selector name=\"name\">a</person-selector></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendPersonSelectorWithInvalidRequiredAttribute() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"required\" of element \"person-selector\" can only be one of the following values: [true, false]");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><person-selector name=\"some-name\" placeholder=\"Person placeholder\" required=\"invalid\"/></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendPersonSelectorWithInvalidAttribute() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"person-selector\"");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><person-selector class=\"some-class\"/></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendPersonSelectorWithBlankName() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><person-selector name=\" \"></person-selector></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }


  @Test
  public void sendPersonSelectorOutsideForm() throws Exception {
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><div><person-selector name=\"some-name\"/></div>" + ACTION_BTN_ELEMENT
        + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element personSelector = form.getChildren().get(0).getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(personSelector.getClass(), PersonSelector.class);
    assertEquals("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR +
        "\"><div><div class=\"person-selector\" data-name=\"some-name\"></div></div>" + ACTION_BTN_ELEMENT + "</form></div>", context.getPresentationML());
    assertEquals("\n   \n(Person Selector)\n\n" + ACTION_BTN_MARKDOWN
        + "\n   \n", context.getMarkdown());
  }

  @Test
  public void testBiContextPersonSelector()
      throws InvalidInputException, IOException, ProcessingException {
    MessageMLContext messageMLContext = new MessageMLContext(null);
    String input = "<messageML>"
        + "<form id=\"form_id\">"
        + "<person-selector name=\"name01\" title=\"title01\" label=\"label01\" "
        + "placeholder=\"placeholder01\" required=\"true\"/>"
        + "<person-selector name=\"name02\" title=\"title02\" "
        + "placeholder=\"placeholder02\"/>"
        + "<button name=\"person-selector\">Submit</button>"
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

    BiItem personSelectorFirstBiItemExpected = new BiItem(BiFields.PERSON_SELECTOR.getValue(),
        expectedAttributesFirst.entrySet()
            .stream()
            .collect(Collectors.toMap(e ->
                String.valueOf(e.getKey()), Map.Entry::getValue)));

    BiItem personSelectorSecondBiItemExpected = new BiItem(BiFields.PERSON_SELECTOR.getValue(),
        expectedAttributesSecond.entrySet()
            .stream()
            .collect(Collectors.toMap(e ->
                String.valueOf(e.getKey()), Map.Entry::getValue)));


    assertEquals(5, items.size());
    assertEquals(BiFields.PERSON_SELECTOR.getValue(), items.get(0).getName());
    assertEquals(BiFields.PERSON_SELECTOR.getValue(), items.get(1).getName());
    assertSameBiItem(personSelectorFirstBiItemExpected, items.get(0));
    assertSameBiItem(personSelectorSecondBiItemExpected, items.get(1));
    assertMessageLengthBiItem(items.get(4), input.length());
  }

  private void assertDataFromValidParsedTag(String dataName, String dataPlaceholder, Boolean dataRequired) {
    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element personSelector = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(personSelector.getClass(), PersonSelector.class);
    assertEquals("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR +
        "\"><div class=\"person-selector\" data-name=\"" + dataName + "\"" +
        (dataPlaceholder != null ? " data-placeholder=\"" + dataPlaceholder + "\"" : "") +
        (dataRequired != null ? " data-required=\"" + dataRequired.toString() + "\"" : "") +
        "></div>" + ACTION_BTN_ELEMENT + "</form></div>", context.getPresentationML());
    String expectedMarkdownText = (dataPlaceholder != null) ? ":[" + addEscapeCharacter(dataPlaceholder) + "]" : "";
    assertEquals("\n   \n(Person Selector" + expectedMarkdownText + ")" + ACTION_BTN_MARKDOWN
        + "\n   \n", context.getMarkdown());
  }

}
