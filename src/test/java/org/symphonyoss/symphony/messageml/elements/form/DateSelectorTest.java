package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.DateSelector;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import static org.junit.Assert.assertEquals;

public class DateSelectorTest extends ElementTest {
  private static final String FORM_ID_ATTR = "id";
  
  @Test
  public void sendValidDateSelectorOnPresentationML() throws Exception {
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR +
        "\"><div class=\"date-selector\" data-name=\"some-name\" data-placeholder=\"some-placeholder\" data-required=\"true\"/>" + ACTION_BTN_ELE + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag("some-name", "some-placeholder", true);
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
        "\"><date-selector name=\"some-name\" placeholder=\"Date placeholder\" required=\"false\"/>" + ACTION_BTN_ELE + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag("some-name", "Date placeholder", false);
  }

  @Test
  public void sendValidDateSelectorWithClosingTag() throws Exception {
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR +
        "\"><date-selector name=\"some-name\"></date-selector>" + ACTION_BTN_ELE + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag("some-name", null, null);
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
        "\"><div><date-selector name=\"some-name\"/></div>" + ACTION_BTN_ELE + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    
    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element dateSelector = form.getChildren().get(0).getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(dateSelector.getClass(), DateSelector.class);
    assertEquals("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR +
        "\"><div><div class=\"date-selector\" data-name=\"some-name\"></div></div>" + ACTION_BTN_ELE + "</form></div>", context.getPresentationML());
    assertEquals("Form (log into desktop client to answer):\n---\n(Date Selector:some-name)\n\n" + ACTION_BTN_MD + "\n---\n", context.getMarkdown());
  }

  private void assertDataFromValidParsedTag(String dataName, String dataPlaceholder, Boolean dataRequired) {
    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element dateSelector = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(dateSelector.getClass(), DateSelector.class);
    assertEquals("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR +
        "\"><div class=\"date-selector\" data-name=\"" + dataName + "\"" + 
        (dataPlaceholder != null ? " data-placeholder=\"" + dataPlaceholder + "\"" : "") +
        (dataRequired != null ? " data-required=\"" + dataRequired.toString() + "\"" : "") +
        "></div>" + ACTION_BTN_ELE + "</form></div>", context.getPresentationML());
    assertEquals("Form (log into desktop client to answer):\n---\n(Date Selector:" + dataName + ")" + ACTION_BTN_MD + "\n---\n", context.getMarkdown());
  }
}