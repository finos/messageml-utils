package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class DropdownTest extends ElementTest{

  @Test
  public void testCompleteRequiredDropdown() throws Exception {
    String id = "complete-required-id";
    String clazz = "symphony-blue";
    boolean required = true;
    String input = "<messageML><form><dropdown class=\"" + clazz + "\" id=\"" + id + "\" required=\"" + required +
            "\"><option></option></dropdown></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element dropdown = form.getChildren().get(0);

    assertEquals("Dropdown class", Dropdown.class, dropdown.getClass());
    verifyDropdownPresentation((Dropdown) dropdown, id, clazz, required);
  }

  @Test
  public void testCompleteNotRequiredDropdown() throws Exception {
    String id = "complete-id";
    String clazz = "symphony-blue";
    boolean required = false;
    String input = "<messageML><form><dropdown class=\"" + clazz + "\" id=\"" + id + "\" required=\"" + required +
            "\"><option></option></dropdown></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element dropdown = form.getChildren().get(0);

    assertEquals("Dropdown class", Dropdown.class, dropdown.getClass());
    verifyDropdownPresentation((Dropdown) dropdown, id, clazz, required);
  }

  @Test
  public void testSimpleDropdown() throws Exception {
    String id = "simple-id";
    String input = "<messageML><form><dropdown id=\"" + id + "\"><option></option></dropdown></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element dropdown = form.getChildren().get(0);

    assertEquals("Dropdown class", Dropdown.class, dropdown.getClass());
    verifyDropdownPresentation((Dropdown) dropdown, id, null, false);
  }

  @Test
  public void testChildlessDropdown() throws Exception {
    String id = "childless-id";
    String input = "<messageML><form><dropdown id=\"" + id + "\"></dropdown></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The \"dropdown\" element must have at least one \"option\" as its child.");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDropdownWithEmptyChild() throws Exception {
    String id = "childless-id";
    String input = "<messageML><form><dropdown id=\"" + id + "\"> </dropdown></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The \"dropdown\" element must have at least one \"option\" as its child.");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDropdownWithoutId() throws Exception {
    String input = "<messageML><form><dropdown><option></option></dropdown></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"id\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }


  @Test
  public void testDropdownWithInvalidRequiredAttribute() throws Exception {
    String id = "invalid-required-id";
    String input = "<messageML><form><dropdown id=\"" + id + "\" required=\"potato\"><option></option></dropdown></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"required\" must be either \"true\" or \"false\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDropdownWithInvalidAttribute() throws Exception {
    String id = "invalid-attribute-id";
    String input = "<messageML><form><dropdown id=\"" + id + "\"  style=\"color: green\"><option></option></dropdown></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"dropdown\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDropdownOutOfForm() throws Exception {
    String id = "out-of-form-id";
    String input = "<messageML><dropdown id=\"" + id + "\"><option></option></dropdown></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("A \"dropdown\" element can only be a child of a \"form\" element");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDropdownWithInvalidChild() throws Exception {
    String id = "invalid-child-id";
    String input = "<messageML><form><dropdown id=\"" + id + "\"><span></span></dropdown></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"span\" is not allowed in \"dropdown\"");

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

  private String getExpectedDropdownPresentation(String id, String clazz, boolean required) {
    return "<div data-format=\"PresentationML\" data-version=\"2.0\"><form><dropdown" + getClassPresentationML(clazz) +
            " id=\"" + id + "\"" + getRequiredPresentationML(required) + "><option></option></dropdown></form></div>";
  }

  private void verifyDropdownPresentation(Dropdown dropdown, String id, String clazz, boolean required) {
    assertEquals("Dropdown id attribute", id, dropdown.getAttribute(Dropdown.ID_ATTR));
    assertEquals("Dropdown class attribute", clazz, dropdown.getAttribute(Dropdown.CLASS_ATTR));
    if (required) {
      assertEquals("Dropdown required attribute", "true", dropdown.getAttribute(Dropdown.REQUIRED_ATTR));
    } else {
      assertNull("Dropdown required attribute", dropdown.getAttribute(Dropdown.REQUIRED_ATTR));
    }

    assertEquals("Dropdown presentationML", getExpectedDropdownPresentation(id, clazz, required),
            context.getPresentationML());
  }
}
