package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.elements.*;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.util.BiFields;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

public class FormTest extends ElementTest {
  private static final String ID_ATTR = "id";
  
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
    expectedException.expectMessage("Elements must have unique ids. The following value is not unique: [" + notUniqueId + "].");
    context.parseMessageML(message, null, MessageML.MESSAGEML_VERSION);
  }


  @Test
  public void testBiContextForm() throws InvalidInputException, IOException, ProcessingException {
    MessageMLContext messageMLContext = new MessageMLContext(null);
    String input = "<messageML>"
        + "<form id=\"all-elements\">"
        + "<button name=\"example-button2\">Button Text</button>"
        + "</form>"
        + "</messageML>";

    messageMLContext.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    List<BiItem> items = messageMLContext.getBiContext().getItems();

    BiItem formBiItemExpected = new BiItem(BiFields.FORM.getFieldName(),
        Collections.singletonMap(BiFields.FORM.getMessageMlAttribute(), 1));

    assertEquals(2, items.size());
    assertSameBiItem(formBiItemExpected, items.get(1));
  }

  private String getExpectedFormPresentationML(Form form) {
    return "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + 
        form.getAttribute(ID_ATTR) + "\"></form></div>";
  }

  private void verifyFormPresentation(Form form, String id) {
    assertEquals(id, form.getAttribute(ID_ATTR));
    assertEquals(getExpectedFormPresentationML(form), context.getPresentationML());
  }


  private String getExpectedFormMarkdown() {
    return String.format("Form (log into desktop client to answer):\n---\n\n---\n");
  }
  
  private void verifyFormMarkdown() {
    String markdown = context.getMarkdown();
    String expectedMarkdown  = getExpectedFormMarkdown();
    assertEquals(expectedMarkdown, markdown);
  }
}
