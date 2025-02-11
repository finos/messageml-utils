package org.finos.symphony.messageml.messagemlutils.elements.form;

import org.finos.symphony.messageml.messagemlutils.elements.*;
import org.junit.Test;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.elements.*;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.exceptions.ProcessingException;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;

public class FormTest extends ElementTest {
  private static final String MESSAGE_ML_WITH_MULTI_SUBMIT = "<messageML>"
      + "<form id=\"formID\" multi-submit=\"reset\">"
      + "<text-field name=\"name_01\" required=\"true\" placeholder=\"Name\"/>"
      + "<button name=\"submit_button\" type=\"action\">Submit</button>"
      + "</form>"
      + "</messageML>";

  @Test
  public void testEmptyForm() {
    String id = "empty-form";
    String input = "<messageML><form id=\"" + id + "\"></form></messageML>";
    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message",
          "The form with id 'empty-form' should have at least one action button", e.getMessage());
    }
  }

  @Test
  public void testFormWithoutId() throws Exception {
    String input = "<messageML><form></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"id\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFormWithBlankId() throws Exception {
    String input = "<messageML><form id=\" \"></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"id\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFormWithInvalidAttribute() throws Exception {
    String id = "invalid-attribute-form";
    String input = "<messageML><form id=\"" + id + "\" invalid=\"true\"></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"invalid\" is not allowed in \"form\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFormWithInvalidChild() throws Exception {
    String id = "invalid-child-form";
    String input = "<messageML><form id=\"" + id + "\"><invalid-child></invalid-child></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid MessageML content at element \"invalid-child\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testNestedForms() throws Exception {
    String input = "<messageML><form id=\"form1\"><div><form id=\"form2\"></form></div></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"form\" cannot be an inner child of the following elements: [form]");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testMultipleFormsUsingSameId() throws Exception {
    String notUniqueId = "id-1";
    String message = "<messageML>"
        + "<form id=\"" + notUniqueId + "\">" + ACTION_BTN_ELEMENT + "</form>"
        + "<div>"
        + "<form id=\"id-2\">" + ACTION_BTN_ELEMENT + "</form>"
        + "<form id=\"" + notUniqueId + "\">" + ACTION_BTN_ELEMENT + "</form>"
        + "</div></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "Elements must have unique ids. The following value is not unique: [" + notUniqueId + "].");
    context.parseMessageML(message, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testWithMultipleDialogs() throws Exception {
    String input = "<messageML>"
        + "<form id=\"id-form\">"
        + "<button name=\"submit\" type=\"action\">submit</button>"
        + "<dialog id=\"id-dialog-one\">"
        + "<title>title</title>"
        + "<body>body</body>"
        + "<footer>footer</footer>"
        + "</dialog>"
        + "<dialog id=\"id-dialog-two\">"
        + "<title>title</title>"
        + "<body>body</body>"
        + "<footer>footer</footer>"
        + "</dialog>"
        + "</form>"
        + "</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    MessageML messageMlInput = context.getMessageML();
    final Element form = messageMlInput.getChild(0);
    assertTrue(form instanceof Form);
    assertEquals(3, form.getChildren().size());
    final Element dialog1 = form.getChild(1);
    final Element dialog2 = form.getChild(2);
    assertTrue(dialog1 instanceof Dialog);
    assertTrue(dialog2 instanceof Dialog);

    final String expectedPattern = "^<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"id-form\">"
        + "<button type=\"action\" name=\"submit\">submit</button>"
        + "<dialog data-width=\"medium\" data-state=\"close\" id=\"\\S+-id-dialog-one\" open=\"\">"
        + "<div class=\"dialog-title\">title</div>"
        + "<div class=\"dialog-body\">body</div>"
        + "<div class=\"dialog-footer\">footer</div>"
        + "</dialog>"
        + "<dialog data-width=\"medium\" data-state=\"close\" id=\"\\S+-id-dialog-two\" open=\"\">"
        + "<div class=\"dialog-title\">title</div>"
        + "<div class=\"dialog-body\">body</div>"
        + "<div class=\"dialog-footer\">footer</div>"
        + "</dialog>"
        + "</form>"
        + "</div>$";
    final String presentationML = context.getPresentationML();
    assertTrue(presentationML.matches(expectedPattern));
  }

  @Test
  public void testMultiSubmitReset() throws InvalidInputException, IOException, ProcessingException {
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<form id=\"formID\" data-multi-submit=\"reset\">"
        + "<input type=\"text\" name=\"name_01\" placeholder=\"Name\" required=\"true\"/>"
        + "<button type=\"action\" name=\"submit_button\">Submit</button>"
        + "</form>"
        + "</div>";
    context.parseMessageML(MESSAGE_ML_WITH_MULTI_SUBMIT, null, MessageML.MESSAGEML_VERSION);
    assertEquals(context.getPresentationML(), expectedPresentationML);
  }

  @Test
  public void testMultiSubmitNotAllowedValue() throws InvalidInputException, IOException, ProcessingException {
    String messageML = "<messageML>"
        + "<form id=\"formID\" multi-submit=\"invalid_value\">"
        + "<text-field name=\"name_01\" required=\"true\" placeholder=\"Name\"/>"
        + "<button name=\"submit_button\" type=\"action\">Submit</button>"
        + "</form>"
        + "</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"multi-submit\" of element \"form\" can only be one "
        + "of the following values: [reset, no-reset].");
    context.parseMessageML(messageML, null, MessageML.MESSAGEML_VERSION);

  }

  @Test
  public void testBiContextWithMultiForm() throws InvalidInputException, IOException, ProcessingException {
    context.parseMessageML(MESSAGE_ML_WITH_MULTI_SUBMIT, null, MessageML.MESSAGEML_VERSION);
    Map<String, Object> textFieldMap = new HashMap<>();
    textFieldMap.put(BiFields.PLACEHOLDER.getValue(), 1);
    textFieldMap.put(BiFields.REQUIRED.getValue(), 1);
    List<BiItem> expectedBiItems =
        Arrays.asList(
            new BiItem(BiFields.TEXT_FIELD.getValue(), textFieldMap),
            new BiItem(BiFields.BUTTON.getValue(), Collections.singletonMap(BiFields.TYPE.getValue(), "action")),
            new BiItem(BiFields.FORM.getValue(), Collections.singletonMap(BiFields.MULTI_SUBMIT.getValue(), 1)),
            new BiItem(BiFields.MESSAGE_LENGTH.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 190)));
    List<BiItem> biItems = context.getBiContext().getItems();
    assertIterableEquals(expectedBiItems, biItems);
  }
}
