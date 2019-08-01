package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.*;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import static org.junit.Assert.assertEquals;

public class FormTest extends ElementTest {
  private static final String ID_ATTR = "id";
  
  @Test
  public void testEmptyForm() throws Exception {
    String id = "empty-form";
    String input = "<messageML><form id=\"" + id + "\"></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);

    assertEquals(Form.class, form.getClass());
    
    verifyFormPresentation((Form) form, id);
    verifyFormMarkdown();
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
    String message = "<messageML><form id=\"id-1\"></form><div><form id=\"id-2\"></form><form id=\"id-1\"></form></div></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Elements must have unique ids.");
    context.parseMessageML(message, null, MessageML.MESSAGEML_VERSION);
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
