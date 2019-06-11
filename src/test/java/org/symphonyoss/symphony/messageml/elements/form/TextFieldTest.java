package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.*;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class TextFieldTest extends ElementTest {

  private static final String NAME_ATTR = "name";
  private static final String REQUIRED_ATTR = "required";
  private static final String PLACEHOLDER_ATTR = "placeholder";


  @Test
  public void testTextField() throws Exception {
    String name = "text-field";
    String input = "<messageML><form><text-field name=\"" + name + "\"/></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    verifyTextFieldPresentation((TextField) textField, name, false, false,false, null);
    verifyTextFieldMarkdown(null);
  }

  @Test
  public void testTextFieldWithAllAttributes() throws Exception {
    String name = "text-field";
    boolean required = true;
    String placeholder = "Input some text here";
    String input = "<messageML><form><text-field name=\"" + name + "\" placeholder=\"" + placeholder + "\" required=\"" + required + "\"/></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    verifyTextFieldPresentation((TextField) textField, name, true, required,true, placeholder);
    verifyTextFieldMarkdown(placeholder);
  }

  @Test
  public void testTextFieldWithAllAttributesPresentationML() throws Exception {
    String name = "text-field";
    boolean required = true;
    String placeholder = "Input some text here";
    String input = "<messageML><form><input type=\"text\" name=\"" + name + "\" placeholder=\"" + placeholder + "\" required=\"" + required + "\"/></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    verifyTextFieldPresentation((TextField) textField, name, true, required,true, placeholder);
    verifyTextFieldMarkdown(placeholder);
  }
  
  @Test
  public void testRequiredTextField() throws Exception {
    String name = "required-text-field";
    boolean required = true;
    String input = "<messageML><form><text-field name=\"" + name + "\" required=\"" + required + "\"/></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    verifyTextFieldPresentation((TextField) textField, name, true, required,false, null);
    verifyTextFieldMarkdown(null);
  }

  @Test
  public void testPlaceholderTextField() throws Exception {
    String name = "placeholder-text-field";
    String placeholder = "Input some text here";
    String input = "<messageML><form><text-field name=\"" + name + "\" placeholder=\"" + placeholder + "\"/></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    verifyTextFieldPresentation((TextField) textField, name, false, false,true, placeholder);
    verifyTextFieldMarkdown(placeholder);
  }

  @Test
  public void testTextFieldWithoutName() throws Exception {
    String input = "<messageML><form><text-field/></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }
  
  @Test
  public void testRequiredTextFieldWithInvalidValue() throws Exception {
    String name = "invalid-required";
    String invalidRequired = "invalidRequired";
    String input = "<messageML><form><text-field name=\"" + name + "\" required=\"" + invalidRequired + "\"/></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"required\" of element \"text-field\" can only be one of the following values: [true, false].");
    
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }  

  private String getRequiredPresentationML(String required) {
    if(required != null) {
      if(required.equals("true") || required.equals("false")) {
        return String.format(" required=\"%s\"", required);
      }
    }

    return "";
  }
  
  private String getExpectedTextFieldPresentationML(TextField textField, boolean shouldShowRequired, boolean shouldShowPlaceholder) {
    return "<div data-format=\"PresentationML\" data-version=\"2.0\"><form><input type=\"text\"" +
        String.format(" name=\"%s\"", textField.getAttribute(NAME_ATTR)) +
        (shouldShowPlaceholder ? String.format(" placeholder=\"%s\"", textField.getAttribute(PLACEHOLDER_ATTR)) : "") +
        (shouldShowRequired ? getRequiredPresentationML(textField.getAttribute(REQUIRED_ATTR)) : "") +
        "/></form></div>";
  }
  
  
  private void verifyTextFieldPresentation(TextField textField, String name, boolean shouldShowRequired, boolean expectedRequired, boolean shouldShowPlaceholder, String expectedPlaceholder) {
    assertEquals("TextField name attribute", name, textField.getAttribute(NAME_ATTR));
    if (shouldShowRequired) {
      assertEquals(String.valueOf(expectedRequired), textField.getAttribute(REQUIRED_ATTR));
    } else {
      assertNull(textField.getAttribute(REQUIRED_ATTR));
    }

    if (shouldShowPlaceholder) {
      assertEquals(String.valueOf(expectedPlaceholder), textField.getAttribute(PLACEHOLDER_ATTR));
    } else {
      assertNull(textField.getAttribute(PLACEHOLDER_ATTR));
    }

    assertEquals(getExpectedTextFieldPresentationML(textField, shouldShowRequired, shouldShowPlaceholder), context.getPresentationML());
  }

  private String getExpectedTextFieldMarkdown(String placeholder) {
    return String.format("Form (log into desktop client to answer):\n---\n(Text Field%s)\n\n---\n", (placeholder != null) ? ":" + placeholder : "");
  }

  private void verifyTextFieldMarkdown(String placeholder) {
    String markdown = context.getMarkdown();
    String expectedMarkdown  = getExpectedTextFieldMarkdown(placeholder);
    assertEquals(expectedMarkdown, markdown);
  }
}
