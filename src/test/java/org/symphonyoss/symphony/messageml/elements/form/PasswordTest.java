package org.symphonyoss.symphony.messageml.elements.form;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.Password;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class PasswordTest extends ElementTest {

  private static final String NAME_VALUE = "field-name";
  private static final String PLACEHOLDER_VALUE = "A placeholder";
  private static final String REQUIRED_VALUE = "true";
  private static final Integer MIN_ALLOWED_LENGTH = 1;
  private static final Integer MAX_ALLOWED_LENGTH = 128;

  private static final String INVALID_MINLENGTH_VALUE_MESSAGE =
      String.format("The attribute \"minlength\" must be between %s and %s", MIN_ALLOWED_LENGTH, MAX_ALLOWED_LENGTH);
  private static final String INVALID_MAXLENGTH_VALUE_MESSAGE =
      String.format("The attribute \"maxlength\" must be between %s and %s", MIN_ALLOWED_LENGTH, MAX_ALLOWED_LENGTH);

  @Test
  public void testPasswordFieldWithRequiredAttributesOnly() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\"/></form></messageML>", NAME_VALUE);
    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\">"
        + "<input type=\"password\" name=\"%s\"/></form></div>", NAME_VALUE);
    String expectedMarkdown = "Form (log into desktop client to answer):\n---\n(Password Field)\n---\n";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element password = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(Password.class, password.getClass());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertTrue("Text should be empty", context.getText().isEmpty());
  }

  @Test
  public void testPasswordFieldWithAllAttributes() throws Exception {
    String input = String.format(
        "<messageML>"
            + "<form id=\"form-id\">"
            + "<password name=\"%s\" placeholder=\"%s\" required=\"%s\" minlength=\"%s\" "
            + "maxlength=\"%s\">password_pw</password>"
            + "</form>"
            + "</messageML>",
        NAME_VALUE, PLACEHOLDER_VALUE, REQUIRED_VALUE, MIN_ALLOWED_LENGTH, MAX_ALLOWED_LENGTH);
    String expectedPresentationML = String.format(
        "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "<form id=\"form-id\">"
            + "<input type=\"password\" name=\"%s\" placeholder=\"%s\" required=\"%s\" "
            + "minlength=\"%s\" maxlength=\"%s\" value=\"password_pw\"/>"
            + "</form>"
            + "</div>",
        NAME_VALUE, PLACEHOLDER_VALUE, REQUIRED_VALUE, MIN_ALLOWED_LENGTH, MAX_ALLOWED_LENGTH);
    String expectedMarkdown = String.format("Form (log into desktop client to answer):\n---\n(Password Field:%s)\n---\n",
        PLACEHOLDER_VALUE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element password = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(Password.class, password.getClass());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertTrue("Text should be empty", context.getText().isEmpty());
  }

  @Test
  public void testPasswordFieldWithPlaceholderAttribute() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" placeholder=\"%s\"/></form></messageML>",
        NAME_VALUE, PLACEHOLDER_VALUE);
    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\">"
            + "<input type=\"password\" name=\"%s\" placeholder=\"%s\"/></form></div>", NAME_VALUE, PLACEHOLDER_VALUE);
    String expectedMarkdown = String.format("Form (log into desktop client to answer):\n---\n(Password Field:%s)\n---\n",
        PLACEHOLDER_VALUE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element password = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(Password.class, password.getClass());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertTrue("Text should be empty", context.getText().isEmpty());
  }

  @Test
  public void testPasswordFieldWithRequiredAttribute() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" required=\"%s\"/></form></messageML>", NAME_VALUE,
        REQUIRED_VALUE);
    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\">"
            + "<input type=\"password\" name=\"%s\" required=\"%s\"/></form></div>", NAME_VALUE, REQUIRED_VALUE);
    String expectedMarkdown = "Form (log into desktop client to answer):\n---\n(Password Field)\n---\n";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element password = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(Password.class, password.getClass());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertTrue("Text should be empty", context.getText().isEmpty());
  }

  @Test
  public void testPasswordFieldWithMinLengthAttribute() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" minlength=\"%s\"/></form></messageML>",
        NAME_VALUE, MIN_ALLOWED_LENGTH);
    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\">"
            + "<input type=\"password\" name=\"%s\" minlength=\"%s\"/></form></div>", NAME_VALUE, MIN_ALLOWED_LENGTH);
    String expectedMarkdown = "Form (log into desktop client to answer):\n---\n(Password Field)\n---\n";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element password = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(Password.class, password.getClass());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertTrue("Text should be empty", context.getText().isEmpty());
  }

  @Test
  public void testPasswordFieldWithMaxLengthAttribute() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" maxlength=\"%s\"/></form></messageML>",
        NAME_VALUE, MAX_ALLOWED_LENGTH);
    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form-id\">"
            + "<input type=\"password\" name=\"%s\" maxlength=\"%s\"/></form></div>", NAME_VALUE, MAX_ALLOWED_LENGTH);
    String expectedMarkdown = "Form (log into desktop client to answer):\n---\n(Password Field)\n---\n";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element password = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(Password.class, password.getClass());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertTrue("Text should be empty", context.getText().isEmpty());
  }

  @Test
  public void testPasswordFieldUsingInputTag() throws Exception {
    String input = String.format(
        "<messageML>"
            + "<form id=\"form-id\">"
            + "<input type=\"password\" value=\"password_pw\" name=\"%s\" placeholder=\"%s\" required=\"%s\" minlength=\"%s\" maxlength=\"%s\"/>"
            + "</form>"
            + "</messageML>",
        NAME_VALUE, PLACEHOLDER_VALUE, REQUIRED_VALUE, MIN_ALLOWED_LENGTH, MAX_ALLOWED_LENGTH);
    String expectedPresentationML = String.format(
        "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "<form id=\"form-id\">"
            + "<input type=\"password\" name=\"%s\" placeholder=\"%s\" required=\"%s\" "
            + "minlength=\"%s\" maxlength=\"%s\" value=\"password_pw\"/>"
            + "</form>"
            + "</div>",
        NAME_VALUE, PLACEHOLDER_VALUE, REQUIRED_VALUE, MIN_ALLOWED_LENGTH, MAX_ALLOWED_LENGTH);
    String expectedMarkdown = String.format("Form (log into desktop client to answer):\n---\n(Password Field:%s)\n---\n",
        PLACEHOLDER_VALUE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element password = form.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    assertEquals(Password.class, password.getClass());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertTrue("Text should be empty", context.getText().isEmpty());
  }

  @Test
  public void testPasswordFieldWithoutRequiredAttributes() throws Exception {
    String input = "<messageML><form id=\"form-id\"><password/></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithInvalidContentMessageML() throws Exception {
    // inserting an attribute only valid for presentation ml in a message ml input
    String messageMLInput = String.format("<messageML><form id=\"form-id\">"
        + "<password name=\"%s\" value=\"value_for_invalid_attr\"/>"
        + "</form></messageML>", NAME_VALUE);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"value\" is not allowed in \"password\"");

    context.parseMessageML(messageMLInput, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithInvalidContentPresentationML() throws Exception {
    // inserting a content only valid for the message ml tag in a presentation ml input
    String presentationMLInput = String.format("<messageML><form id=\"form-id\">"
        + "<input type=\"password\" name=\"%s\">Text</input>"
        + "</form></messageML>", NAME_VALUE);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"password\" may not have child elements or text content");

    context.parseMessageML(presentationMLInput, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithInvalidRequiredValue() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" required=\"yes\"/></form></messageML>",
        NAME_VALUE);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "Attribute \"required\" of element \"password\" can only be one of the following values: [true, false].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithNonNumericMinLength() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" minlength=\"1A\"/></form></messageML>",
        NAME_VALUE);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(INVALID_MINLENGTH_VALUE_MESSAGE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithNonIntegerMinLength() throws Exception {
    String input =
        String.format("<messageML><form id=\"form-id\"><password name=\"%s\" minlength=\"1.1\"/></form></messageML>", NAME_VALUE);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(INVALID_MINLENGTH_VALUE_MESSAGE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithMinLengthLowerThanAllowed() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" minlength=\"%s\"/></form></messageML>", NAME_VALUE,
        MIN_ALLOWED_LENGTH - 1);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(INVALID_MINLENGTH_VALUE_MESSAGE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithMinLengthGreaterThanAllowed() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" minlength=\"%s\"/></form></messageML>", NAME_VALUE,
        MAX_ALLOWED_LENGTH + 1);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(INVALID_MINLENGTH_VALUE_MESSAGE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithNonNumericMaxLength() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" maxlength=\"1A\"/></form></messageML>", NAME_VALUE);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(INVALID_MAXLENGTH_VALUE_MESSAGE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithNonIntegerMaxLength() throws Exception {
    String input =
        String.format("<messageML><form id=\"form-id\"><password name=\"%s\" maxlength=\"1.1\"/></form></messageML>", NAME_VALUE);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(INVALID_MAXLENGTH_VALUE_MESSAGE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithMaxLengthLowerThanAllowed() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" maxlength=\"%s\"/></form></messageML>", NAME_VALUE,
        MIN_ALLOWED_LENGTH - 1);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(INVALID_MAXLENGTH_VALUE_MESSAGE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithMaxLengthGreaterThanAllowed() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" maxlength=\"%s\"/></form></messageML>", NAME_VALUE,
        MAX_ALLOWED_LENGTH + 1);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(INVALID_MAXLENGTH_VALUE_MESSAGE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPasswordFieldWithMinLengthGreaterThanMaxLength() throws Exception {
    String input = String.format("<messageML><form id=\"form-id\"><password name=\"%s\" minlength=\"%s\" maxlength=\"%s\"/></form></messageML>",
            NAME_VALUE, MAX_ALLOWED_LENGTH, MAX_ALLOWED_LENGTH - 1);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"minlength\" must be lower than the \"maxlength\" attribute");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

}
