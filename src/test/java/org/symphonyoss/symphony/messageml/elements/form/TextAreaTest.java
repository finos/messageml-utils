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
import org.symphonyoss.symphony.messageml.elements.TextArea;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TextAreaTest extends ElementTest {

  private static final String NAME_VALUE = "A name";
  private static final String PLACEHOLDER_VALUE = "A placeholder";
  private static final String INITIAL_VALUE = "An initial value";
  private static final String INITIAL_VALUE_WITH_LINE_BREAK = "An \n initial \n value";
  private static final String LABEL = "label here";
  private static final String TITLE = "title here";

  private static final String EXPECTED_MARKDOWN = "Form (log into desktop client to answer):\n---\n(Text Area)" + ACTION_BTN_MARKDOWN
      + "\n---\n";
  private static final String EXPECTED_MARKDOWN_WITH_PLACEHOLDER =
      String.format("Form (log into desktop client to answer):\n---\n(Text Area:[%s])" + ACTION_BTN_MARKDOWN + "\n---\n", PLACEHOLDER_VALUE);
  private static final String EXPECTED_MARKDOWN_WITH_INITIAL_VALUE =
      String.format("Form (log into desktop client to answer):\n---\n(Text Area:%s)" + ACTION_BTN_MARKDOWN + "\n---\n", INITIAL_VALUE);
  private static final String EXPECTED_MARKDOWN_WITH_LINE_BREAK =
      String.format("Form (log into desktop client to answer):\n---\n(Text Area:%s)" + ACTION_BTN_MARKDOWN + "\n---\n", INITIAL_VALUE_WITH_LINE_BREAK);
  private static final String EXPECTED_MARKDOWN_WITH_PLACEHOLDER_INITIAL_VALUE_LABEL_TITLE =
      String.format("Form (log into desktop client to answer):\n---\n(Text Area:[%s][%s][%s]%s)" + ACTION_BTN_MARKDOWN + "\n---\n", PLACEHOLDER_VALUE, LABEL, TITLE, INITIAL_VALUE);

  @Rule
  public ExpectedException exceptionRule = ExpectedException.none();
  
  @Test
  public void testTextAreaWithRequiredAttributesOnly() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><textarea name=\"%s\"></textarea>%s</form></messageML>", NAME_VALUE,
        ACTION_BTN_ELEMENT);
    String expectedPresentationML =
        String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\"><textarea name=\"%s\"></textarea>%s</form></div>",
            NAME_VALUE, ACTION_BTN_ELEMENT);

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
        String.format("<messageML><form id=\"form-id\"><textarea name=\"%s\" placeholder=\"%s\" required=\"true\" minlength=\"3\" maxlength=\"25\" label=\"%s\" title=\"%s\">%s</textarea>%s</form></messageML>",
            NAME_VALUE, PLACEHOLDER_VALUE, LABEL, TITLE, INITIAL_VALUE, ACTION_BTN_ELEMENT);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    String presentationML = context.getPresentationML();
    String textAreaRegex = ".*(\"textarea-(.*?)\").*";
    Pattern pattern = Pattern.compile(textAreaRegex);
    Matcher matcher = pattern.matcher(presentationML);
    String uniqueLabelId = matcher.matches() ? matcher.group(2) : null;

    String expectedPresentationML = String.format(
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\">"
            + "<div class=\"textarea-group\" data-generated=\"true\">"
            + "<label for=\"textarea-%s\">label here</label>"
            + "<span class=\"info-hint\" data-target-id=\"textarea-%s\" data-title=\"title here\"></span>"
            + "<textarea maxlength=\"25\" minlength=\"3\" name=\"%s\" placeholder=\"%s\" "
            + "required=\"true\" id=\"textarea-%s\">%s</textarea></div>%s</form></div>",
        uniqueLabelId, uniqueLabelId, NAME_VALUE, PLACEHOLDER_VALUE, uniqueLabelId, INITIAL_VALUE, ACTION_BTN_ELEMENT);


    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());
    assertEquals("Markdown", EXPECTED_MARKDOWN_WITH_PLACEHOLDER_INITIAL_VALUE_LABEL_TITLE, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, presentationML);
    assertEquals("Text should be the initial value", INITIAL_VALUE, textArea.getChild(0).asText());
  }

  @Test
  public void testTextAreaWithInitialValue() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><textarea name=\"%s\">%s</textarea>%s</form></messageML>", NAME_VALUE, INITIAL_VALUE,
        ACTION_BTN_ELEMENT);
    String expectedPresentationML =
        String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\"><textarea name=\"%s\">%s</textarea>%s</form></div>",
            NAME_VALUE, INITIAL_VALUE, ACTION_BTN_ELEMENT);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());
    assertEquals("Markdown", EXPECTED_MARKDOWN_WITH_INITIAL_VALUE, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Text should be the initial value", INITIAL_VALUE, textArea.getChild(0).asText());
  }

  @Test
  public void testTextAreaWithLineBreak() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><textarea name=\"%s\">%s</textarea>%s</form></messageML>", NAME_VALUE, INITIAL_VALUE_WITH_LINE_BREAK,
            ACTION_BTN_ELEMENT);
    String expectedPresentationML =
            String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\"><textarea name=\"%s\">%s</textarea>%s</form></div>",
                    NAME_VALUE, INITIAL_VALUE_WITH_LINE_BREAK, ACTION_BTN_ELEMENT);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textArea = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(TextArea.class, textArea.getClass());
    assertEquals("Markdown", EXPECTED_MARKDOWN_WITH_LINE_BREAK, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Text should be the initial value", INITIAL_VALUE_WITH_LINE_BREAK, textArea.getChild(0).asText());
  }

  @Test
  public void testTextAreaRequiredAttribute() throws Exception {
    String input =
        String.format(
            "<messageML><form id=\"form-id\"><textarea name=\"%s\" required=\"true\"></textarea>%s</form></messageML>", NAME_VALUE,
            ACTION_BTN_ELEMENT);
    String expectedPresentationML = String.format(
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\"><textarea name=\"%s\" required=\"true\"></textarea>%s</form></div>",
        NAME_VALUE, ACTION_BTN_ELEMENT);

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
    String input = String.format(
        "<messageML><form id=\"form-id\"><textarea name=\"%s\" placeholder=\"%s\"></textarea>%s</form></messageML>", NAME_VALUE,
        PLACEHOLDER_VALUE, ACTION_BTN_ELEMENT);
    String expectedPresentationML = String.format(
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\"><textarea name=\"%s\" placeholder=\"%s\"></textarea>%s</form></div>",
        NAME_VALUE, PLACEHOLDER_VALUE, ACTION_BTN_ELEMENT);

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

  @Test
  public void testTextAreaWithRegex() throws Exception {
    String input = String.format(
        "<messageML><form id=\"form-id\"><textarea name=\"%s\" pattern=\"regex\" pattern-error-message=\"Regex Error\"></textarea>%s</form></messageML>", NAME_VALUE,
        ACTION_BTN_ELEMENT);
    String expectedPresentationML =
        String.format(
            "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\"><textarea name=\"%s\" pattern=\"regex\" data-pattern-error-message=\"Regex Error\"></textarea>%s</form></div>",
            NAME_VALUE, ACTION_BTN_ELEMENT);

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
  public void testTextAreaWithInvalidRegex() throws Exception {
    String invalidRegex = "[abc+";

    exceptionRule.expect(InvalidInputException.class);
    exceptionRule.expectMessage(String.format(RegexElement.REGEX_NOT_VALID_ERR, invalidRegex));

    String input = String.format(
        "<messageML><form id=\"form-id\"><textarea name=\"%s\" pattern=\"%s\" pattern-error-message=\"Regex Error\"></textarea></form></messageML>",
        NAME_VALUE,
        invalidRegex);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaWithMissingPatternError() throws Exception {

    exceptionRule.expect(InvalidInputException.class);
    exceptionRule.expectMessage(String.format(RegexElement.ATTRIBUTE_MANDATORY_WHEN_ATTRIBUTE_DEFINED_ERR, RegexElement.PATTERN_ERROR_MESSAGE_ATTR, RegexElement.PATTERN_ATTR));

    String input = String.format(
        "<messageML><form id=\"form-id\"><textarea name=\"%s\" pattern=\"%s\"></textarea></form></messageML>",
        NAME_VALUE, "");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaPatternTooLong() throws Exception {
    String regexTooLong = StringUtils.leftPad("", RegexElement.PATTERN_MAX_LENGTH + 1);

    exceptionRule.expect(InvalidInputException.class);
    exceptionRule.expectMessage(String.format(RegexElement.ATTRIBUTE_TOO_LONG_ERR, RegexElement.PATTERN_ATTR, RegexElement.PATTERN_MAX_LENGTH));

    String input = String.format(
        "<messageML><form id=\"form-id\"><textarea name=\"%s\" pattern=\"%s\" pattern-error"
            + "-message=\"Regex Error\"></textarea></form></messageML>",
        NAME_VALUE,
        regexTooLong);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaPatternErrorTooLong() throws Exception {
    String errTooLong = StringUtils.leftPad("", RegexElement.PATTERN_ERROR_MESSAGE_MAX_LENGTH + 1);

    exceptionRule.expect(InvalidInputException.class);
    exceptionRule.expectMessage(String.format(RegexElement.ATTRIBUTE_TOO_LONG_ERR, RegexElement.PATTERN_ERROR_MESSAGE_ATTR, RegexElement.PATTERN_ERROR_MESSAGE_MAX_LENGTH));

    String input = String.format(
        "<messageML><form id=\"form-id\"><textarea name=\"%s\" pattern=\"regex\" pattern-error"
            + "-message=\"%s\"></textarea></form></messageML>",
        NAME_VALUE,
        errTooLong);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaMessageMLWithDefaultValueBiggerThanMaxLength() throws Exception {
    String input = "<messageML>"
            + "<form id=\"form-id\">"
            + "<textarea name=\"sample name\" maxlength=\"5\" minlength=\"1\">Value here</textarea>"
            + "</form>"
            + "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The length of this textarea's initial value must be between 1 and 5");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaMessageMLWithDefaultValueSmallerThanMinLength() throws Exception {
    String input = "<messageML>"
            + "<form id=\"form-id\">"
            + "<textarea name=\"sample name\" maxlength=\"20\" minlength=\"5\">Text</textarea>"
            + "</form>"
            + "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The length of this textarea's initial value must be between 5 and 20");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaMessageMLWithValueSmallerThanMinLength() throws Exception {
    String input = "<messageML>"
            + "<form id=\"form-id\">"
            + "<textarea name=\"sample name\" maxlength=\"20\" minlength=\"-3\">Text</textarea>"
            + "</form>"
            + "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"minlength\" must be between 0 and 10000");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTextAreaMessageMLWithValueSmallerThanMaxLength() throws Exception {
    String input = "<messageML>"
            + "<form id=\"form-id\">"
            + "<textarea name=\"sample name\" maxlength=\"20000\" minlength=\"3\">Text</textarea>"
            + "</form>"
            + "</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"maxlength\" must be between 0 and 10000");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }
}
