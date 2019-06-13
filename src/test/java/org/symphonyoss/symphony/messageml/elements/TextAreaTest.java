package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.symphonyoss.symphony.messageml.elements.TextArea.VALID_VALUES_FOR_REQUIRED_ATTR;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class TextAreaTest extends ElementTest {

  private static final String NAME_ATTR = "name";
  private static final String REQUIRED_ATTR = "required";
  private static final String PLACEHOLDER_ATTR = "placeholder";

  private static final String NAME_VALUE = "A name";
  private static final String PLACEHOLDER_VALUE = "A placeholder";
  private static final String INITIAL_VALUE = "An initial value";

  private static final String EXPECTED_MARKDOWN = "Form (log into desktop client to answer):\n---\n(Text Area)\n\n---\n";
  private static final String EXPECTED_MARKDOWN_WITH_PLACEHOLDER =
      String.format("Form (log into desktop client to answer):\n---\n(Text Area:%s)\n\n---\n", PLACEHOLDER_VALUE);

  @Test
  public void testTextAreaWithRequiredAttributesOnly() throws Exception {
    String input = String.format("<messageML><form><textarea name=\"%s\"></textarea></form></messageML>", NAME_VALUE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());

    verifyTextAreaPresentationML((TextArea) textArea, false, false, false);

    assertEquals(EXPECTED_MARKDOWN, context.getMarkdown());
    assertTrue("Text should be empty", context.getText().isEmpty());
  }

  @Test
  public void testTextAreaWithAllAttributes() throws Exception {
    String input =
        String.format("<messageML><form><textarea name=\"%s\" placeholder=\"%s\" required=\"true\">%s</textarea></form></messageML>",
            NAME_VALUE, PLACEHOLDER_VALUE, INITIAL_VALUE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());

    verifyTextAreaPresentationML((TextArea) textArea, true, true, true);

    assertEquals(EXPECTED_MARKDOWN_WITH_PLACEHOLDER, context.getMarkdown());
    assertEquals("Text should be the initial value", INITIAL_VALUE, context.getText());
  }

  @Test
  public void testTextAreaWithInitialValue() throws Exception {
    String input = String.format("<messageML><form><textarea name=\"%s\">%s</textarea></form></messageML>", NAME_VALUE, INITIAL_VALUE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());

    verifyTextAreaPresentationML((TextArea) textArea, true, false, false);

    assertEquals(EXPECTED_MARKDOWN, context.getMarkdown());
    assertEquals("Text should be the initial value", INITIAL_VALUE, context.getText());
  }

  @Test
  public void testTextAreaRequiredAttribute() throws Exception {
    String input =
        String.format("<messageML><form><textarea name=\"%s\" required=\"true\"></textarea></form></messageML>", NAME_VALUE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());

    verifyTextAreaPresentationML((TextArea) textArea, false, true, false);

    assertEquals(EXPECTED_MARKDOWN, context.getMarkdown());
    assertTrue("Text should be empty", context.getText().isEmpty());
  }

  @Test
  public void testTextAreaPlaceholderAttribute() throws Exception {
    String input = String.format("<messageML><form><textarea name=\"%s\" placeholder=\"%s\"></textarea></form></messageML>", NAME_VALUE,
        PLACEHOLDER_VALUE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());

    verifyTextAreaPresentationML((TextArea) textArea, false, false, true);

    assertEquals(EXPECTED_MARKDOWN_WITH_PLACEHOLDER, context.getMarkdown());
    assertTrue("Text should be empty", context.getText().isEmpty());
  }

  @Test
  public void testTextAreaWithoutRequiredFields() throws Exception {
    String input = "<messageML><form><textarea></textarea></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaWithInvalidRequiredAttributeValue() throws Exception {
    String input = String.format("<messageML><form><textarea name=\"%s\" required=\"value\"></textarea></form></messageML>", NAME_VALUE);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "Attribute \"required\" of element \"textarea\" can only be one of the following values: [true, false].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaWithNotAllowedAttribute() throws Exception {
    String attribute = "anotherAttribute";
    String input = String.format("<messageML><form><textarea %s=\"value\"></textarea></form></messageML>", attribute);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"" + attribute + "\" is not allowed in \"textarea\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaWithChildren() throws Exception {
    String childElement = "div";
    String initialValueWithChildren = String.format("<%s>%s</%s>", childElement, INITIAL_VALUE, childElement);
    String input = String.format("<messageML><form><textarea name=\"%s\">%s</textarea></form></messageML>", NAME_VALUE,
        initialValueWithChildren);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"" + childElement + "\" is not allowed in \"textarea\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  private void verifyTextAreaPresentationML(TextArea textArea, boolean shouldRenderInitialValue, boolean shouldRenderRequired,
      boolean shouldRenderPlaceholder) {
    String nameValue = String.format(" name=\"%s\"", textArea.getAttribute(NAME_ATTR));
    String placeholderValue = shouldRenderPlaceholder ? String.format(" placeholder=\"%s\"", textArea.getAttribute(PLACEHOLDER_ATTR)) : "";
    String requiredValue = shouldRenderRequired ? getRequiredTextAreaPresentationML(textArea.getAttribute(REQUIRED_ATTR)) : "";
    String initialValue = shouldRenderInitialValue ? INITIAL_VALUE : "";

    String expectedPresentationML =
        String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form><textarea%s%s%s>%s</textarea></form></div>",
            nameValue, placeholderValue, requiredValue, initialValue);

    assertEquals(expectedPresentationML, context.getPresentationML());
  }

  private String getRequiredTextAreaPresentationML(String requiredValue) {
    if (VALID_VALUES_FOR_REQUIRED_ATTR.contains(requiredValue)) {
      return String.format(" required=\"%s\"", requiredValue);
    }

    return "";
  }
}
