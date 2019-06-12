package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
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

  @Test
  public void testTextAreaWithRequiredAttributesOnly() throws Exception {
    String input = String.format("<messageML><form><textarea name=\"%s\"></textarea></form></messageML>", NAME_VALUE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());

    verifyTextAreaPresentationML((TextArea) textArea, NAME_VALUE, false, false, false, false, null);
    verifyTextAreaMarkdown(null);
  }

  @Test
  public void testTextAreaWithAllAttributes() throws Exception {
    boolean requiredValue = true;
    String input =
        String.format("<messageML><form><textarea name=\"%s\" placeholder=\"%s\" required=\"%s\">%s</textarea></form></messageML>",
            NAME_VALUE, PLACEHOLDER_VALUE, requiredValue, INITIAL_VALUE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());

    verifyTextAreaPresentationML((TextArea) textArea, NAME_VALUE, true, true, requiredValue, true, PLACEHOLDER_VALUE);
    verifyTextAreaMarkdown(PLACEHOLDER_VALUE);
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

    verifyTextAreaPresentationML((TextArea) textArea, NAME_VALUE, true, false, false, false, null);
    verifyTextAreaMarkdown(null);
  }

  @Test
  public void testTextAreaRequiredAttribute() throws Exception {
    boolean requiredValue = true;
    String input =
        String.format("<messageML><form><textarea name=\"%s\" required=\"%s\"></textarea></form></messageML>", NAME_VALUE, requiredValue);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());

    verifyTextAreaPresentationML((TextArea) textArea, NAME_VALUE, false, true, requiredValue, false, null);
    verifyTextAreaMarkdown(null);
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

    verifyTextAreaPresentationML((TextArea) textArea, NAME_VALUE, false, false, false, true, PLACEHOLDER_VALUE);
    verifyTextAreaMarkdown(PLACEHOLDER_VALUE);
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

  private void verifyTextAreaPresentationML(TextArea textArea, String name, boolean shouldRenderInitialValue, boolean shouldRenderRequired,
      boolean expectedRequired, boolean shouldRenderPlaceholder, String expectedPlaceholder) {

    String nameValue = String.format(" name=\"%s\"", textArea.getAttribute(NAME_ATTR));
    String placeholderValue = shouldRenderPlaceholder ? String.format(" placeholder=\"%s\"", textArea.getAttribute(PLACEHOLDER_ATTR)) : "";
    String requiredValue = shouldRenderRequired ? getRequiredTextAreaPresentationML(textArea.getAttribute(REQUIRED_ATTR)) : "";
    String initialValue = shouldRenderInitialValue ? INITIAL_VALUE : "";

    String expectedPresentationML =  String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form><textarea%s%s%s>%s</textarea></form></div>",
        nameValue, placeholderValue, requiredValue, initialValue);

    assertEquals(expectedPresentationML, context.getPresentationML());
  }

  private void verifyTextAreaMarkdown(String placeholder) {
    String innerValue = (placeholder != null) ? ":" + placeholder : "";
    String expectedMarkdown = String.format("Form (log into desktop client to answer):\n---\n(Text Area%s)\n\n---\n", innerValue);

    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
  }

  private String getRequiredTextAreaPresentationML(String requiredValue) {
    if (VALID_VALUES_FOR_REQUIRED_ATTR.contains(requiredValue)) {
      return String.format(" required=\"%s\"", requiredValue);
    }

    return "";
  }

}
