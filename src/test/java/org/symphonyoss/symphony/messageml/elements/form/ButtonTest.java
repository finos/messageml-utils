package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.Button;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import static org.symphonyoss.symphony.messageml.elements.Button.ACTION_TYPE;
import static org.symphonyoss.symphony.messageml.elements.Button.PRIMARY_CLASS;
import static org.symphonyoss.symphony.messageml.elements.Button.RESET_TYPE;

public class ButtonTest extends ElementTest {

  private static final String NAME_ATTR = "name";
  private static final Set<String> VALID_CLASSES = new HashSet<>(Arrays.asList("primary", "secondary",
      "primary-destructive", "secondary-destructive"));
  private static final String TYPE_ATTR = "type";
  private static final String CLASS_ATTR = "class";
  private static final String FORM_ID_ATTR = "text-field-form";

  @Test
  public void testCompleteButton() throws Exception {
    String type = ACTION_TYPE;
    String name = "action-btn-name";
    String clazz = PRIMARY_CLASS;
    String innerText = "Complete";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\" class=\"" + clazz + "\" name=\"" + name + "\">"
            + innerText + "</button></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element button = form.getChildren().get(0);

    assertEquals("Button class", Button.class, button.getClass());
    verifyButtonPresentation((Button) button, name, type, clazz, innerText);
  }

  @Test
  public void testResetButton() throws Exception {
    String type = RESET_TYPE;
    String innerText = "Reset";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\">" + innerText +
        "</button>" + ACTION_BTN_ELE + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element button = form.getChildren().get(0);

    assertEquals("Button class", Button.class, button.getClass());
    verifyButtonPresentation((Button) button,null, type, null, innerText);
  }

  @Test
  public void testSendingResetButtonWithoutActionButton() {
    String type = RESET_TYPE;
    String innerText = "Reset";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\">" + innerText +
        "</button></form></messageML>";
    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message",
          "The form with id 'text-field-form' should have at least one action button", e.getMessage());
    }
  }

  @Test
  public void testTypelessButtonWithName() throws Exception {
    String innerText = "Typeless Button With Name";
    String name = "btn-name";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button name=\"" + name + "\">" + innerText + "</button></form></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element button = form.getChildren().get(0);

    assertEquals("Button class", Button.class, button.getClass());
    verifyButtonPresentation((Button) button, name, "action", null, innerText);
  }

  @Test
  public void testActionButtonWithName() throws Exception {
    String type = ACTION_TYPE;
    String name = "btnName";
    String innerText = "Action Button With Name";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\" name=\"" + name + "\">" + innerText
            + "</button></form></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element button = form.getChildren().get(0);

    assertEquals("Button class", Button.class, button.getClass());
    verifyButtonPresentation((Button) button, name, type, null, innerText);
  }

  @Test
  public void testButtonWithValidClasses() throws Exception {
    String type = RESET_TYPE;
    String innerText = "Class Test";

    for (String clazz : VALID_CLASSES) {
      String input =
          "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\" class=\"" + clazz + "\">"
              + innerText
              + "</button>" + ACTION_BTN_ELE + "</form></messageML>";
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

      Element messageML = context.getMessageML();
      Element form = messageML.getChildren().get(0);
      Element button = form.getChildren().get(0);

      assertEquals("Button class", Button.class, button.getClass());
      verifyButtonPresentation((Button) button, null, type, clazz, innerText);
    }
  }

  @Test
  public void testTypelessButtonWithoutName() throws Exception {
    String innerText = "Typeless Button Without Name";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button>" + innerText + "</button></form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on typeless button without name");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Attribute \"name\" is required for action buttons", e.getMessage());
    }
  }

  @Test
  public void testActionButtonWithoutName() throws Exception {
    String type = ACTION_TYPE;
    String innerText = "Typeless Button Without Name";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\">" + innerText + "</button></form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on action button without name");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Attribute \"name\" is required for action buttons", e.getMessage());
    }
  }

  @Test
  public void testActionButtonWithBlankName() throws Exception {
    String type = ACTION_TYPE;
    String innerText = "Typeless Button Without Name";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\" name=\" \">" + innerText + "</button></form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on action button without name");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Attribute \"name\" is required for action buttons", e.getMessage());
    }
  }

  @Test
  public void testResetButtonWithName() throws Exception {
    String type = RESET_TYPE;
    String innerText = "Reset";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\" name=\" \">" + innerText + "</button></form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on action button without name");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Attribute \"name\" is allowed for action buttons only", e.getMessage());
    }
  }

  @Test
  public void testActionButtonWithoutTextNode() throws Exception {
    String type = ACTION_TYPE;
    String name = "btn-name";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\" name=\"" + name + "\"></button></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The \"button\" element must have at least one child that is any of the following elements: [text content].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testMisplacedButton() throws Exception {
    String innerText = "Misplaced Button";
    String type = RESET_TYPE;
    String input = "<messageML><button type=\"" + type + "\">" + innerText + "</button></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on button out of a form tag");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Element \"button\" can only be a inner child of the following elements: [form]", e.getMessage());
    }
  }

  @Test
  public void testBadTypeButton() throws Exception {
    String innerText = "Invalid Type Button";
    String type = "potato";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\">" + innerText + "</button></form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on invalid type button");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Attribute \"type\" must be \"action\" or \"reset\"", e.getMessage());
    }
  }

  @Test
  public void testBadClassButton() throws Exception {
    String innerText = "Invalid Class Button";
    String type = RESET_TYPE;
    String clazz = "outclassed";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\" class=\"" + clazz + "\">" + innerText
            + "</button></form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on invalid class button");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Attribute \"class\" must be \"primary\", \"secondary\", " +
              "\"primary-destructive\" or \"secondary-destructive\"", e.getMessage());
    }
  }

  @Test
  public void testBadAttributeButton() throws Exception {
    String innerText = "Invalid Attribute Button";
    String type = RESET_TYPE;
    String invalidAttribute = "invalid-attribute";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + type + "\" " + invalidAttribute + "=\"invalid\">" + innerText
            + "</button></form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on invalid attribute of button");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Attribute \"" + invalidAttribute + "\" is not allowed in \""
              + Button.MESSAGEML_TAG + "\"", e.getMessage());
    }
  }
  
  @Test
  public void testNotDirectParent() throws Exception {
    String input = "<messageML><form id=\"example\"><div><button name=\"div-button\">SELECT</button></div></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element button = form.getChildren().get(0).getChildren().get(0);

    assertEquals("Button class", Button.class, button.getClass());
    
    assertEquals("Button name attribute", "div-button", button.getAttribute(NAME_ATTR));
    assertEquals("Button type attribute", "action", button.getAttribute(TYPE_ATTR));
    assertEquals("Button clazz attribute", null, button.getAttribute(CLASS_ATTR));
    assertEquals("Button inner text", "SELECT", button.getChild(0).asText());

    assertEquals("Button markdown", "Form (log into desktop client to answer):\n---\n(Button:SELECT)\n\n\n---\n", context.getMarkdown());
    assertEquals("Button presentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"example\"><div><button type=\"action\" name=\"div-button\">SELECT</button></div></form></div>",
        context.getPresentationML());
  }

  private String getNamePresentationML(String name) {
    if (name != null) {
      return " name=\"" + name + "\"";
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

  private String getExpectedButtonPresentation(String name, String type, String clazz, String innerText, Boolean shouldHaveAdditionalStandardActionBtn) {
    if (shouldHaveAdditionalStandardActionBtn) {
      return "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR
          + "\"><button type=\"" + type + "\"" + getClassPresentationML(clazz) + getNamePresentationML(name) + ">"
          + innerText + "</button>" + ACTION_BTN_ELE + "</form></div>";
    } else {
      return "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR
          + "\"><button type=\"" + type + "\"" + getClassPresentationML(clazz) + getNamePresentationML(name) + ">"
          + innerText + "</button></form></div>";
    }
  }

  private String getExpectedButtonMarkdown(String innerText, Boolean shouldHaveAdditionalStandardActionBtn) {
    if (shouldHaveAdditionalStandardActionBtn) {
      return "Form (log into desktop client to answer):\n---\n(Button:"+ innerText + ")" + ACTION_BTN_MD + "\n---\n";
    } else {
      return "Form (log into desktop client to answer):\n---\n(Button:"+ innerText + ")\n---\n";
    }
  }

  private void verifyButtonPresentation(Button button, String name, String type, String clazz, String innerText) {
    assertEquals("Button name attribute", name, button.getAttribute(NAME_ATTR));
    assertEquals("Button type attribute", type, button.getAttribute(TYPE_ATTR));
    assertEquals("Button clazz attribute", clazz, button.getAttribute(CLASS_ATTR));
    assertEquals("Button inner text", innerText, button.getChild(0).asText());

    Boolean isResetType = type.equals(RESET_TYPE);
    assertEquals("Button markdown", getExpectedButtonMarkdown(innerText, isResetType), context.getMarkdown());
    assertEquals("Button presentationML",
        getExpectedButtonPresentation(name, type, clazz, innerText, isResetType), context.getPresentationML());
  }
}
