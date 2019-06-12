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
  @Test
  public void sendValidDateSelectorOnPresentationML() throws Exception {
    context.parseMessageML("<messageML><form><div class=\"date-selector\"/></form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag();
  }

  @Test
  public void sendInvalidAttrDateSelectorOnPresentationML() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"id\" is not allowed in \"date-selector\"");
    context.parseMessageML("<messageML><form><div id=\"idOne\" class=\"date-selector\"/></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendInvalidContentDateSelectorOnPresentationML() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"date-selector\" may not have child elements or text content");
    context.parseMessageML("<messageML><form><div class=\"date-selector\"><div>hey</div></div></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendValidDateSelector() throws Exception {
    context.parseMessageML("<messageML><form><date-selector/></form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag();
  }

  @Test
  public void sendValidDateSelectorWithClosingTag() throws Exception {
    context.parseMessageML("<messageML><form><date-selector></date-selector></form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag();
  }

  @Test
  public void sendDateSelectorWithChildElement() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"date-selector\" may not have child elements or text content");
    context.parseMessageML("<messageML><form><date-selector>a</date-selector></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendDateSelectorWithAttribute() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"date-selector\" may not have attributes");
    context.parseMessageML("<messageML><form><date-selector class=\"some-class\"/></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendDateSelectorOutsideForm() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"date-selector\" can only be a child of the following elements: [form]");
    context.parseMessageML("<messageML><form><div><date-selector/></div></form></messageML>", null, MessageML.MESSAGEML_VERSION);
  }

  private void assertDataFromValidParsedTag() {
    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element dateSelector = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(dateSelector.getClass(), DateSelector.class);
    assertEquals("<div data-format=\"PresentationML\" data-version=\"2.0\"><form><div class=\"date-selector\"/></form></div>", context.getPresentationML());
    assertEquals("Form (log into desktop client to answer):\n---\n(Date Selector)\n\n---\n", context.getMarkdown());
  }
}