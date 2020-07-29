package org.symphonyoss.symphony.messageml.elements.form;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.apache.commons.lang3.StringUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.RegexElement;
import org.symphonyoss.symphony.messageml.elements.TextField;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TextFieldTest extends ElementTest {

  private static final String FORM_ID_ATTR = "text-field-form";

  @Rule
  public ExpectedException exceptionRule = ExpectedException.none();

  @Test
  public void testTextField() throws Exception {
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"text-field\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\"/>"
        + ACTION_BTN_ELEMENT
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
    verifyTextFieldMarkdown(null, null, null, null);
  }

  @Test
  public void testTextFieldWithAllAttributes() throws Exception {
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"text-field\" label=\"label text here\" title=\"title here\" "
        + "placeholder=\"Input some text here\" required=\"true\" minlength=\"10\" maxlength=\"20\"/>"
        + ACTION_BTN_ELEMENT
        + "</form>"
        + "</messageML>";
    
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textField.getClass());

    String presentationML = context.getPresentationML();
    String textFieldRegex = ".*(\"textfield-(.*?)\").*";
    Pattern pattern = Pattern.compile(textFieldRegex);
    Matcher matcher = pattern.matcher(presentationML);
    String uniqueLabelId = matcher.matches() ? matcher.group(2) : null;

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<div class=\"textfield-group\" data-generated=\"true\">"
        + "<label for=\"textfield-" + uniqueLabelId + "\">label text here</label>"
        + "<span class=\"info-hint\" data-target-id=\"textfield-" + uniqueLabelId + "\" data-title=\"title here\"></span>"
        + "<input type=\"text\" name=\"text-field\" placeholder=\"Input some text here\" required=\"true\" minlength=\"10\" maxlength=\"20\" id=\"textfield-" + uniqueLabelId + "\"/>"
        + "</div>"
        + ACTION_BTN_ELEMENT
        + "</form>"
        + "</div>";
    
    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, presentationML);
    verifyTextFieldMarkdown("Input some text here", null, "label text here", "title here");
  }

  @Test
  public void testTextFieldWithAllAttributesPresentationML() throws Exception {
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"text-field\" placeholder=\"Input some text here\" required=\"true\" minlength=\"10\" maxlength=\"20\"/>"
        + ACTION_BTN_ELEMENT
        + "</form>"
        + "</messageML>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" placeholder=\"Input some text here\" required=\"true\" minlength=\"10\" maxlength=\"20\"/>"
        + ACTION_BTN_ELEMENT
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
    verifyTextFieldMarkdown("Input some text here", null, null, null);
  }

  @Test
  public void testMaskedTextField() throws Exception {
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"text-field\" masked=\"true\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" data-masked=\"true\"/>"
        + ACTION_BTN_ELEMENT
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
    verifyTextFieldMarkdown(null, null, null, null);
  }

  @Test
  public void testNonMaskedTextField() throws Exception {
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"text-field\" masked=\"false\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" data-masked=\"false\"/>"
        + ACTION_BTN_ELEMENT
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
    verifyTextFieldMarkdown(null, null, null, null);
  }

  @Test
  public void testNonMaskedTextFieldWithPlaceholderAndInitialValue() throws Exception {
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"text-field\" masked=\"false\" placeholder=\"Input some text...\">Initial value</text-field>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" placeholder=\"Input some text...\" data-masked=\"false\" value=\"Initial value\"/>"
        + ACTION_BTN_ELEMENT
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
    verifyTextFieldMarkdown("Input some text...", "Initial value", null, null);
  }

  @Test
  public void testMaskedTextFieldPresentationML() throws Exception {
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"text-field\" data-masked=\"true\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" data-masked=\"true\"/>"
        + ACTION_BTN_ELEMENT
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
    verifyTextFieldMarkdown(null, null, null, null);
  }

  @Test
  public void testNonMaskedTextFieldPresentationML() throws Exception {
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"text-field\" data-masked=\"false\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"text-field\" data-masked=\"false\"/>"
        + ACTION_BTN_ELEMENT
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
    verifyTextFieldMarkdown(null, null, null, null);
  }

  @Test
  public void testTextFieldInvalidAttrPresentationML() throws Exception {
    String input =
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form\"><input type=\"text\" name=\"name1\" invalid=\"invalid\" placeholder=\"placeholder1\" required=\"true\"/><button type=\"action\" name=\"send-answers\">Submit</button></form></div>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"invalid\" is not allowed in \"text-field\"");

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
        + ACTION_BTN_ELEMENT
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
        + ACTION_BTN_ELEMENT
        + "</form>"
        + "</div>";
    assertEquals("The parsed content should be equivalent to the expected presentation ML",
        expectedPresentationML, context.getPresentationML());
    verifyTextFieldMarkdown(null, "Value here", null, null);
  }

  @Test
  public void testTextFieldPresentationMLWithDefaultValue() throws Exception {
    String txtFieldName = "sample name";
    String input = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"" + txtFieldName + "\" value=\"value\"/>"
        + ACTION_BTN_ELEMENT
        + "</form>"
        + "</messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"sample name\" value=\"value\"/>"
        + ACTION_BTN_ELEMENT
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
    verifyTextFieldMarkdown(null, "value", null, null);
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
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"" + name + "\" required=\"" + required + "\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"required-text-field\" required=\"true\"/>"
        + ACTION_BTN_ELEMENT
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
    verifyTextFieldMarkdown(null, null, null, null);
  }

  @Test
  public void testPlaceholderTextField() throws Exception {
    String name = "placeholder-text-field";
    String placeholder = "Input some text here";
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"" + name + "\" placeholder=\"" + placeholder + "\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"text-field-form\">"
        + "<input type=\"text\" name=\"placeholder-text-field\" placeholder=\"Input some text here\"/>"
        + ACTION_BTN_ELEMENT
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
    verifyTextFieldMarkdown(placeholder, null, null, null);
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

  @Test
  public void testTextFieldWithRegex() throws Exception {
    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"text-field\" pattern=\"regex\" pattern-error-message=\"Regex Error\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<input type=\"text\" name=\"text-field\" pattern=\"regex\" "
        + "data-pattern-error-message=\"Regex Error\"/>"
        + ACTION_BTN_ELEMENT
        + "</form>"
        + "</div>";

    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextField.class, textArea.getClass());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertTrue("Text should be empty", textArea.getChildren().isEmpty());
  }

  @Test
  public void testTextFieldWithInvalidRegex() throws Exception {
    String invalidRegex = "[abc+";

    exceptionRule.expect(InvalidInputException.class);
    exceptionRule.expectMessage(String.format(RegexElement.REGEX_NOT_VALID_ERR, invalidRegex));

    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"text-field\" pattern=\"" + invalidRegex + "\" pattern-error-message=\"Regex Error\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldWithMissingPatternError() throws Exception {

    exceptionRule.expect(InvalidInputException.class);
    exceptionRule.expectMessage(String.format(RegexElement.ATTRIBUTE_MANDATORY_WHEN_ATTRIBUTE_DEFINED_ERR, RegexElement.PATTERN_ERROR_MESSAGE_ATTR, RegexElement.PATTERN_ATTR));

    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"text-field\" pattern=\"\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldPatternTooLong() throws Exception {
    String regexTooLong = StringUtils.leftPad("", RegexElement.PATTERN_MAX_LENGTH + 1);

    exceptionRule.expect(InvalidInputException.class);
    exceptionRule.expectMessage(String.format(RegexElement.ATTRIBUTE_TOO_LONG_ERR, RegexElement.PATTERN_ATTR, RegexElement.PATTERN_MAX_LENGTH));

    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"text-field\" pattern=\"" + regexTooLong + "\" pattern-error-message=\"Regex Error\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextFieldPatternErrorTooLong() throws Exception {
    String errTooLong = StringUtils.leftPad("", RegexElement.PATTERN_ERROR_MESSAGE_MAX_LENGTH + 1);

    exceptionRule.expect(InvalidInputException.class);
    exceptionRule.expectMessage(String.format(RegexElement.ATTRIBUTE_TOO_LONG_ERR, RegexElement.PATTERN_ERROR_MESSAGE_ATTR, RegexElement.PATTERN_ERROR_MESSAGE_MAX_LENGTH));

    String messageMLInput = "<messageML>"
        + "<form id=\"" + FORM_ID_ATTR + "\">"
        + "<text-field name=\"text-field\" pattern=\"\" pattern-error-message=\"" + errTooLong + "\"/>"
        + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);
  }

  private String getExpectedTextFieldMarkdown(String placeholder, String initialValue, String label, String title) {
    String expectedMarkdownText = ((placeholder != null) ? "[" + placeholder + "]" : "") +
        ((label != null) ? "[" + label + "]" : "") +
        ((title != null) ? "[" + title + "]" : "") +
        ((initialValue != null) ? initialValue : "");

    return String.format("Form (log into desktop client to answer):\n---\n(Text Field%s)"+ ACTION_BTN_MARKDOWN + "\n---\n", (!expectedMarkdownText.isEmpty()) ? ":" + expectedMarkdownText : "");
  }

  private void verifyTextFieldMarkdown(String placeholder, String initialValue, String label, String title) {
    String markdown = context.getMarkdown();
    String expectedMarkdown  = getExpectedTextFieldMarkdown(placeholder, initialValue, label, title);
    assertEquals(expectedMarkdown, markdown);
  }
}
