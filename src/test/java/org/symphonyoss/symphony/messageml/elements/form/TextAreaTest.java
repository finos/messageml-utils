package org.symphonyoss.symphony.messageml.elements.form;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.TextArea;
import org.symphonyoss.symphony.messageml.elements.TextNode;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class TextAreaTest extends ElementTest {

  private static final String NAME_VALUE = "A name";
  private static final String PLACEHOLDER_VALUE = "A placeholder";
  private static final String INITIAL_VALUE = "An initial value";

  private static final String EXPECTED_MARKDOWN = "Form (log into desktop client to answer):\n---\n(Text Area)" + ACTION_BTN_MD + "\n---\n";
  private static final String EXPECTED_MARKDOWN_WITH_PLACEHOLDER =
      String.format("Form (log into desktop client to answer):\n---\n(Text Area:%s)" + ACTION_BTN_MD + "\n---\n", PLACEHOLDER_VALUE);

  @Test
  public void testTextAreaWithRequiredAttributesOnly() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><textarea name=\"%s\"></textarea>%s</form></messageML>", NAME_VALUE, ACTION_BTN_ELE);
    String expectedPresentationML =
        String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\"><textarea name=\"%s\"></textarea>%s</form></div>",
            NAME_VALUE, ACTION_BTN_ELE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());
    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertTrue("Text should be empty", textArea.getChildren().isEmpty());
  }

  @Test
  public void testTextAreaWithAllAttributes() throws Exception {
    String input =
        String.format("<messageML><form id=\"form-id\"><textarea name=\"%s\" placeholder=\"%s\" required=\"true\">%s</textarea>%s</form></messageML>",
            NAME_VALUE, PLACEHOLDER_VALUE, INITIAL_VALUE, ACTION_BTN_ELE);
    String expectedPresentationML = String.format(
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\"><textarea name=\"%s\" placeholder=\"%s\" "
            + "required=\"true\">%s</textarea>%s</form></div>",
        NAME_VALUE, PLACEHOLDER_VALUE, INITIAL_VALUE, ACTION_BTN_ELE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());
    assertEquals("Markdown", EXPECTED_MARKDOWN_WITH_PLACEHOLDER, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Text should be the initial value", INITIAL_VALUE, textArea.getChild(0).asText());
  }

  @Test
  public void testTextAreaWithInitialValue() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><textarea name=\"%s\">%s</textarea>%s</form></messageML>", NAME_VALUE, INITIAL_VALUE, ACTION_BTN_ELE);
    String expectedPresentationML =
        String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\"><textarea name=\"%s\">%s</textarea>%s</form></div>",
            NAME_VALUE, INITIAL_VALUE, ACTION_BTN_ELE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());
    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Text should be the initial value", INITIAL_VALUE, textArea.getChild(0).asText());
  }

  @Test
  public void testTextAreaRequiredAttribute() throws Exception {
    String input =
        String.format("<messageML><form id=\"form-id\"><textarea name=\"%s\" required=\"true\"></textarea>%s</form></messageML>", NAME_VALUE, ACTION_BTN_ELE);
    String expectedPresentationML = String.format(
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\"><textarea name=\"%s\" required=\"true\"></textarea>%s</form></div>",
        NAME_VALUE, ACTION_BTN_ELE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());
    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertTrue("Text should be empty", textArea.getChildren().isEmpty());
  }

  @Test
  public void testTextAreaPlaceholderAttribute() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><textarea name=\"%s\" placeholder=\"%s\"></textarea>%s</form></messageML>", NAME_VALUE,
        PLACEHOLDER_VALUE, ACTION_BTN_ELE);
    String expectedPresentationML = String.format(
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\"><textarea name=\"%s\" placeholder=\"%s\"></textarea>%s</form></div>",
        NAME_VALUE, PLACEHOLDER_VALUE, ACTION_BTN_ELE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());
    assertEquals("Markdown", EXPECTED_MARKDOWN_WITH_PLACEHOLDER, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertTrue("Text should be empty", textArea.getChildren().isEmpty());
  }

  @Test
  public void testTextAreaWithoutRequiredFields() throws Exception {
    String input = "<messageML><form id=\"form-id\"><textarea></textarea></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaWithBlankName() throws Exception {
    String input = "<messageML><form id=\"form-id\"><textarea name=\" \"></textarea></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaWithInvalidRequiredAttributeValue() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><textarea name=\"%s\" required=\"value\"></textarea></form></messageML>", NAME_VALUE);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "Attribute \"required\" of element \"textarea\" can only be one of the following values: [true, false].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaWithNotAllowedAttribute() throws Exception {
    String attribute = "anotherAttribute";
    String input = String.format("<messageML><form id=\"form-id\"><textarea %s=\"value\"></textarea></form></messageML>", attribute);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"" + attribute + "\" is not allowed in \"textarea\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaWithChildren() throws Exception {
    String childElement = "div";
    String initialValueWithChildren = String.format("<%s>%s</%s>", childElement, INITIAL_VALUE, childElement);
    String input = String.format("<messageML><form id=\"form-id\"><textarea name=\"%s\">%s</textarea></form></messageML>", NAME_VALUE,
        initialValueWithChildren);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"" + childElement + "\" is not allowed in \"textarea\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

}
