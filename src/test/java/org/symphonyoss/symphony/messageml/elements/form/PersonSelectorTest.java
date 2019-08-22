package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.PersonSelector;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import static org.junit.Assert.assertEquals;

public class PersonSelectorTest extends ElementTest {
  private static final String FORM_ID_ATTR = "id";
  
  @Test
  public void sendValidPersonSelectorOnPresentationML() throws Exception {
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><div class=\"person-selector\" data-name=\"one-name\" data-placeholder=\"some-placeholder\" data-required=\"true\"/>" + ACTION_BTN_ELE + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag("one-name", "some-placeholder", true);
  }

  @Test
  public void sendInvalidAttrPersonSelectorOnPresentationML() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"id\" is not allowed in \"person-selector\"");
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><div id=\"idOne\" class=\"person-selector\" data-name=\"any-name\"/></form></messageML>", null, MessageML.MESSAGEML_VERSION);
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
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><person-selector name=\"" + name + "\" placeholder=\"Add some user here...\" required=\"false\"/>" + ACTION_BTN_ELE + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
    assertDataFromValidParsedTag(name, "Add some user here...", false);
  }

  @Test
  public void sendValidPersonSelectorWithClosingTag() throws Exception {
    String name = "other-name";
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><person-selector name=\"" + name +"\" placeholder=\"Add some user here...\"></person-selector>" + ACTION_BTN_ELE + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);
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
    context.parseMessageML("<messageML><form id=\"" + FORM_ID_ATTR + "\"><div><person-selector name=\"some-name\"/></div>" + ACTION_BTN_ELE + "</form></messageML>", null, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element personSelector = form.getChildren().get(0).getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(personSelector.getClass(), PersonSelector.class);
    assertEquals("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR +
        "\"><div><div class=\"person-selector\" data-name=\"some-name\"></div></div>" + ACTION_BTN_ELE + "</form></div>", context.getPresentationML());
    assertEquals("Form (log into desktop client to answer):\n---\n(Person Selector:some-name)\n\n" + ACTION_BTN_MD + "\n---\n", context.getMarkdown());
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
        "></div>" + ACTION_BTN_ELE + "</form></div>", context.getPresentationML());
    assertEquals("Form (log into desktop client to answer):\n---\n(Person Selector:" + dataName + ")" + ACTION_BTN_MD + "\n---\n", context.getMarkdown());
  }
}