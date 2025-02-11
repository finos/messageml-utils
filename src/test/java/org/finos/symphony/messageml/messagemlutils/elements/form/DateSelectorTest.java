package org.finos.symphony.messageml.messagemlutils.elements.form;

import org.junit.Test;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.elements.DateSelector;
import org.finos.symphony.messageml.messagemlutils.elements.Element;
import org.finos.symphony.messageml.messagemlutils.elements.ElementTest;
import org.finos.symphony.messageml.messagemlutils.elements.Form;
import org.finos.symphony.messageml.messagemlutils.elements.MessageML;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.exceptions.ProcessingException;

import static org.junit.Assert.assertEquals;
import static org.finos.symphony.messageml.messagemlutils.markdown.MarkdownRenderer.addEscapeCharacter;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DateSelectorTest extends ElementTest {
  private static final String FORM_ID_ATTR = "id";
  
  @Test
  public void sendValidDateSelectorOnPresentationML() throws Exception {
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR +
        "\"><div class=\"date-selector\" data-name=\"some-name\" data-placeholder=\"some-placeholder\" data-required=\"true\"/>" + ACTION_BTN_ELEMENT
        + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag("some-name", "some-placeholder", true, null);
  }

  @Test
  public void sendInvalidAttrDateSelectorOnPresentationML() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"id\" is not allowed in \"date-selector\"");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><div id=\"idOne\" class=\"date-selector\" data-name=\"some-name\"/></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendInvalidContentDateSelectorOnPresentationML() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"date-selector\" may not have child elements or text content");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><div class=\"date-selector\" data-name=\"some-name\"><div>hey</div></div></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendValidDateSelector() throws Exception {
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR +
        "\"><date-selector name=\"some-name\" placeholder=\"Date placeholder\" required=\"false\" formnovalidate=\"true\"/>" + ACTION_BTN_ELEMENT
        + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag("some-name", "Date placeholder", false, true);
  }

  @Test
  public void sendValidDateSelectorWithClosingTag() throws Exception {
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR +
        "\"><date-selector name=\"some-name\"></date-selector>" + ACTION_BTN_ELEMENT + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag("some-name", null, null, null);
  }

  @Test
  public void sendDateSelectorWithChildElement() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"date-selector\" may not have child elements or text content");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><date-selector name=\"some-name\">a</date-selector></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendDateSelectorWithInvalidRequiredAttribute() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"required\" of element \"date-selector\" can only be one of the following values: [true, false]");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><date-selector name=\"some-name\" placeholder=\"Date placeholder\" required=\"invalid\"/></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendDateSelectorWithInvalidAttribute() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"date-selector");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><date-selector name=\"some-name\" class=\"some-class\"/></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendDateSelectorOutsideForm() throws Exception {
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR +
        "\"><div><date-selector name=\"some-name\"/></div>" + ACTION_BTN_ELEMENT + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    
    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element dateSelector = form.getChildren().get(0).getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(dateSelector.getClass(), DateSelector.class);
    assertEquals("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR +
        "\"><div><div class=\"date-selector\" data-name=\"some-name\"></div></div>" + ACTION_BTN_ELEMENT + "</form></div>", context.getPresentationML());
    assertEquals("\n   \n(Date Selector)\n\n" + ACTION_BTN_MARKDOWN + "\n   \n",
        context.getMarkdown());
  }

  @Test
  public void testBiContextDateSelector()
      throws InvalidInputException, IOException, ProcessingException {
    MessageMLContext messageMLContext = new MessageMLContext(null);
    String input = "<messageML>"
        + "<form id=\"form_id\">"

        + "<date-selector name=\"rules\" "
        + "placeholder=\"placeholder01\" required=\"true\"/>"

        + "<button name=\"date-picker\">Submit</button>"
        + "</form>"
        + "</messageML>";

    messageMLContext.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    List<BiItem> items = messageMLContext.getBiContext().getItems();

    Map<Object, Object> expectedAttributes = Stream.of(new Object[][] {
        {BiFields.PLACEHOLDER.getValue(), 1},
        {BiFields.REQUIRED.getValue(), 1},
    }).collect(Collectors.toMap(property -> property[0], property -> property[1]));

    BiItem datePickerBiItemExpected = new BiItem(BiFields.DATE_SELECTOR.getValue(),
        expectedAttributes.entrySet()
            .stream()
            .collect(Collectors.toMap(e ->
                String.valueOf(e.getKey()), Map.Entry::getValue)));
    BiItem formBiItemExpected = new BiItem(BiFields.FORM.getValue(), Collections.emptyMap());

    assertEquals(4, items.size());
    assertEquals(BiFields.DATE_SELECTOR.getValue(), items.get(0).getName());
    assertSameBiItem(datePickerBiItemExpected, items.get(0));
    assertSameBiItem(formBiItemExpected, items.get(2));
    assertMessageLengthBiItem(items.get(3), input.length());
  }

  private void assertDataFromValidParsedTag(String dataName, String dataPlaceholder, Boolean dataRequired,
      Boolean formnovalidate) {
    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element dateSelector = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(dateSelector.getClass(), DateSelector.class);
    assertEquals("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR +
        "\"><div class=\"date-selector\" data-name=\"" + dataName + "\"" + 
        (dataPlaceholder != null ? " data-placeholder=\"" + dataPlaceholder + "\"" : "") +
        (dataRequired != null ? " data-required=\"" + dataRequired.toString() + "\"" : "") +
        (formnovalidate != null ? String.format(" data-formnovalidate=\"%s\"",formnovalidate) : "") +
        "></div>" + ACTION_BTN_ELEMENT + "</form></div>", context.getPresentationML());
    String expectedMarkdownText = (dataPlaceholder != null) ? ":[" + addEscapeCharacter(dataPlaceholder) + "]" : "";
    assertEquals("\n   \n(Date Selector" + expectedMarkdownText + ")" + ACTION_BTN_MARKDOWN
        + "\n   \n", context.getMarkdown());
  }

}
