package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class SelectTest extends ElementTest{

  @Test
  public void testCompleteRequiredSelect() throws Exception {
    String id = "complete-required-id";
    String clazz = "symphony-blue";
    boolean required = true;
    String input = "<messageML><form><select class=\"" + clazz + "\" id=\"" + id + "\" required=\"" + required +
            "\"><option></option></select></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, id, clazz, required);
  }

  @Test
  public void testCompleteNotRequiredSelect() throws Exception {
    String id = "complete-id";
    String clazz = "symphony-blue";
    boolean required = false;
    String input = "<messageML><form><select class=\"" + clazz + "\" id=\"" + id + "\" required=\"" + required +
            "\"><option></option></select></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, id, clazz, required);
  }

  @Test
  public void testSimpleSelect() throws Exception {
    String id = "simple-id";
    String input = "<messageML><form><select id=\"" + id + "\"><option></option></select></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, id, null, false);
  }

  @Test
  public void testChildlessSelect() throws Exception {
    String id = "childless-id";
    String input = "<messageML><form><select id=\"" + id + "\"></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The \"select\" element must have at least one \"option\" as its child.");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectWithEmptyChild() throws Exception {
    String id = "childless-id";
    String input = "<messageML><form><select id=\"" + id + "\"> </select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The \"select\" element must have at least one \"option\" as its child.");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectWithoutId() throws Exception {
    String input = "<messageML><form><select><option></option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"id\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }


  @Test
  public void testSelectWithInvalidRequiredAttribute() throws Exception {
    String id = "invalid-required-id";
    String input = "<messageML><form><select id=\"" + id + "\" required=\"potato\"><option></option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"required\" must be either \"true\" or \"false\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectWithInvalidAttribute() throws Exception {
    String id = "invalid-attribute-id";
    String input = "<messageML><form><select id=\"" + id + "\"  style=\"color: green\"><option></option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"select\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectOutOfForm() throws Exception {
    String id = "out-of-form-id";
    String input = "<messageML><select id=\"" + id + "\"><option></option></select></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("A \"select\" element can only be a child of a \"form\" element");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectWithInvalidChild() throws Exception {
    String id = "invalid-child-id";
    String input = "<messageML><form><select id=\"" + id + "\"><span></span></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"span\" is not allowed in \"select\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  private String getRequiredPresentationML(boolean required) {
    if (required) {
      return " required=\"true\"";
    } else {
      return "";
    }
  }

  private String getClassPresentationML(String clazz) {
    if (clazz != null) {
      return " class=\"" + clazz + "\"";
    } else {
      return "";
    }
  }

  private String getExpectedSelectPresentation(String id, String clazz, boolean required) {
    return "<div data-format=\"PresentationML\" data-version=\"2.0\"><form><select" + getClassPresentationML(clazz) +
            " id=\"" + id + "\"" + getRequiredPresentationML(required) + "><option></option></select></form></div>";
  }

  private void verifySelectPresentation(Select select, String id, String clazz, boolean required) {
    assertEquals("Select id attribute", id, select.getAttribute(Select.ID_ATTR));
    assertEquals("Select class attribute", clazz, select.getAttribute(Select.CLASS_ATTR));
    if (required) {
      assertEquals("Select required attribute", "true", select.getAttribute(Select.REQUIRED_ATTR));
    } else {
      assertNull("Select required attribute", select.getAttribute(Select.REQUIRED_ATTR));
    }

    assertEquals("Select presentationML", getExpectedSelectPresentation(id, clazz, required),
            context.getPresentationML());
  }
}
