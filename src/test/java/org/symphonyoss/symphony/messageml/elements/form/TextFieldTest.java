package org.symphonyoss.symphony.messageml.elements.form;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.TextField;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class TextFieldTest extends ElementTest {

  private static final String FORM_ID_ATTR = "text-field-form";


  @Test
  public void testTextField() throws Exception {
    String messageMLInput = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><text-field name=\"text-field\"/></form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\"/>"
        + "</form>"
        + "</div>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown(null);
  }

  @Test
  public void testTextFieldWithAllAttributes() throws Exception {
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"text-field\" placeholder=\"Input some text here\" required=\"true\" minlength=\"10\" maxlength=\"20\"/>"
        + "</form>"
        + "</messageML>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" placeholder=\"Input some text here\" required=\"true\" minlength=\"10\" maxlength=\"20\"/>"
        + "</form>"
        + "</div>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown("Input some text here");
  }

  @Test
  public void testTextFieldWithAllAttributesPresentationML() throws Exception {
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"text-field\" placeholder=\"Input some text here\" required=\"true\" minlength=\"10\" maxlength=\"20\"/>"
        + "</form>"
        + "</messageML>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" placeholder=\"Input some text here\" required=\"true\" minlength=\"10\" maxlength=\"20\"/>"
        + "</form>"
        + "</div>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown("Input some text here");
  }

  @Test
  public void testMaskedTextField() throws Exception {
    String messageMLInput = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><text-field name=\"text-field\" masked=\"true\"/></form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" data-masked=\"true\"/>"
        + "</form>"
        + "</div>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown(null);
  }

  @Test
  public void testNonMaskedTextField() throws Exception {
    String messageMLInput = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><text-field name=\"text-field\" masked=\"false\"/></form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" data-masked=\"false\"/>"
        + "</form>"
        + "</div>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown(null);
  }

  @Test
  public void testMaskedTextFieldPresentationML() throws Exception {
    String messageMLInput = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><input type=\"text\" name=\"text-field\" data-masked=\"true\"/></form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" data-masked=\"true\"/>"
        + "</form>"
        + "</div>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown(null);
  }

  @Test
  public void testNonMaskedTextFieldPresentationML() throws Exception {
    String messageMLInput = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><input type=\"text\" name=\"text-field\" data-masked=\"false\"/></form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" data-masked=\"false\"/>"
        + "</form>"
        + "</div>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown(null);
  }

  @Test
  public void testTextFieldInvalidAttrPresentationML() throws Exception {
    String input = "<messageML><form><input type=\"text\" name=\"name1\" id=\"id1\" placeholder=\"placeholder1\" required=\"true\"/></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"id\" is not allowed in \"text-field\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldInvalidAttribute() throws Exception {
    String input = "<messageML><form><text-field name=\"name1\" id=\"id1\" placeholder=\"placeholder1\" required=\"true\" /></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"id\" is not allowed in \"text-field\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldInvalidAttributeFromPresentationMLElement() throws Exception {
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"name1\" value=\"value1\" placeholder=\"placeholder1\" required=\"true\"></text-field>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"value\" is not allowed in \"text-field\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldInvalidContent() throws Exception {
    // test if it breaks when we put a text field inside a text field, should only accept simple text content within
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"first_attribute\" placeholder=\"placeholder1\">"
        + "<text-field name=\"INVALID_NESTED_ATTRIBUTE\" placeholder=\"placeholder1\"/>"
        + "</text-field></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"text-field\" is not allowed in \"text-field\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldInvalidContentPresentationML() throws Exception {
    // test if it breaks when we put an input type text inside an input type text, it should not accept anything inside.
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"first_attribute\" placeholder=\"placeholder1\" required=\"true\">"
        + "<input type=\"text\" name=\"INVALID_NESTED_ATTRIBUTE\"/></input>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"text-field\" may not have child elements or text content");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldInvalidContentPresentationMLTextNode() throws Exception {
    // test if it breaks when we put an input type text inside an input type text, it should not accept anything inside.
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"first_attribute\" placeholder=\"placeholder1\" required=\"true\">some value</input>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"text-field\" may not have child elements or text content");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldMessageMLWithDefaultValue() throws Exception {
    String input = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"sample name\">Value here</text-field>"
        + "</form>"
        + "</messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    String expectedPresentationML = ""
        + "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"sample name\" value=\"Value here\"/>"
        + "</form>"
        + "</div>";
    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown(null);
  }

  @Test
  public void testTextFieldPresentationMLWithDefaultValue() throws Exception {
    String txtFieldName = "sample name";
    String input = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"" + txtFieldName + "\" value=\"value\"/>"
        + "</form>"
        + "</messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"sample name\" value=\"value\"/>"
        + "</form>"
        + "</div>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown(null);
  }

  @Test
  public void testTextFieldMessageMLWithDefaultValueBiggerThanMaxLength() throws Exception {
    String input = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"sample name\" maxlength=\"5\">Value here</text-field>"
        + "</form>"
        + "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The length of this text-field's initial value must be between 1 and 5");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldMessageMLWithDefaultValueBiggerThanMinLength() throws Exception {
    String input = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"sample name\" minlength=\"20\">Value here</text-field>"
        + "</form>"
        + "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The length of this text-field's initial value must be between 20 and 128");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldMessageMLWithMinAndMaxLengthWithTheSameValue() throws Exception {
    String input = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"sample name\" maxlength=\"5\" minlength=\"5\">Value here</text-field>"
        + "</form>"
        + "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The length of this text-field's initial value must be between 5 and 5");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldPresentationMLWithDefaultValueBiggerThanMaxLength() throws Exception {
    String input = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"sample name\" maxlength=\"5\" value=\"Value here\" />"
        + "</form>"
        + "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The length of this text-field's initial value must be between 1 and 5");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldPresentationMLWithDefaultValueBiggerThanMinLength() throws Exception {
    String input = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"sample name\" minlength=\"20\" value=\"Value here\" />"
        + "</form>"
        + "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The length of this text-field's initial value must be between 20 and 128");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldPresentationMLWithMinAndMaxLengthWithTheSameValue() throws Exception {
    String input = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"sample name\" maxlength=\"5\" minlength=\"5\" value=\"Value here\"/>"
        + "</form>"
        + "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The length of this text-field's initial value must be between 5 and 5");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testRequiredTextField() throws Exception {
    String name = "required-text-field";
    boolean required = true;
    String messageMLInput = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><text-field name=\"" + name + "\" required=\"" + required + "\"/></form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"required-text-field\" required=\"true\"/>"
        + "</form>"
        + "</div>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown(null);
  }

  @Test
  public void testPlaceholderTextField() throws Exception {
    String name = "placeholder-text-field";
    String placeholder = "Input some text here";
    String messageMLInput = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><text-field name=\"" + name + "\" placeholder=\"" + placeholder + "\"/></form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"placeholder-text-field\" placeholder=\"Input some text here\"/>"
        + "</form>"
        + "</div>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown(placeholder);
  }

  @Test
  public void testTextFieldWithoutName() throws Exception {
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><text-field/></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldWithBlankName() throws Exception {
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><text-field name=\" \"/></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testRequiredTextFieldWithInvalidValue() throws Exception {
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><text-field name=\"invalid-required\" required=\"invalidRequired\"/></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"required\" of element \"text-field\" can only be one of the following values: [true, false].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testLengthAttributeNotNumber() throws Exception {
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><text-field name=\"invalid-number\" maxlength=\"notNumber\"/></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"maxlength\" must be a valid number.");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testLengthAttributeMinBiggerThanMaxNumber() throws Exception {
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><text-field name=\"invalid-max-min\" minlength=\"30\" maxlength=\"2\"/></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"minlength\" must be lower than the \"maxlength\" attribute");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  private String getExpectedTextFieldMarkdown(String placeholder) {
    return String.format("Form (log into desktop client to answer):\n---\n(Text Field%s)\n---\n", (placeholder != null) ? ":" + placeholder : "");
  }

  private void verifyTextFieldMarkdown(String placeholder) {
    String markdown = context.getMarkdown();
    String expectedMarkdown  = getExpectedTextFieldMarkdown(placeholder);
    assertEquals(expectedMarkdown, markdown);
  }
}
