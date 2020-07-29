package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.PersonSelector;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.Assert.assertEquals;

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
    assertEquals("Form (log into desktop client to answer):\n---\n(Person Selector:" + expectedMarkdownText + ")" + ACTION_BTN_MARKDOWN
        + "\n---\n", context.getMarkdown());
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
    assertEquals("Form (log into desktop client to answer):\n---\n(Person Selector)\n\n" + ACTION_BTN_MARKDOWN
        + "\n---\n", context.getMarkdown());
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
    String expectedMarkdownText = (dataPlaceholder != null) ? ":[" + dataPlaceholder + "]" : "";
    assertEquals("Form (log into desktop client to answer):\n---\n(Person Selector" + expectedMarkdownText + ")" + ACTION_BTN_MARKDOWN
        + "\n---\n", context.getMarkdown());
  }
}
