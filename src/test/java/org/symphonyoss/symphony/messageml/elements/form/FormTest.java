package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.*;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

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


}
