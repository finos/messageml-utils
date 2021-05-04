package org.symphonyoss.symphony.messageml.elements.form;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.symphonyoss.symphony.messageml.elements.Button.ACTION_TYPE;
import static org.symphonyoss.symphony.messageml.elements.Button.RESET_TYPE;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.elements.Button;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ButtonTest extends ElementTest {

  private static final String NAME_ATTR = "name";
  private static final Set<String> VALID_CLASSES = new HashSet<>(Arrays.asList("primary", "secondary",
      "primary-destructive", "secondary-destructive", "tertiary", "destructive")); // primary-destructive, secondary-destructive are deprecated
  private static final String TYPE_ATTR = "type";
  private static final String CLASS_ATTR = "class";
  private static final String FORM_ID_ATTR = "text-field-form";

  @Test
  public void testCompleteButtonInForm() throws Exception {
    String type = ACTION_TYPE;
    String name = "action-btn-name";
    String clazz = "primary";
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
        "</button>" + ACTION_BTN_ELEMENT + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element button = form.getChildren().get(0);

    assertEquals("Button class", Button.class, button.getClass());
    verifyButtonPresentation((Button) button,null, type, null, innerText);
  }

  @Test
  public void testSendingResetButtonWithoutActionButton() {
    String innerText = "Reset";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + RESET_TYPE + "\">" + innerText +
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
              + "</button>" + ACTION_BTN_ELEMENT + "</form></messageML>";
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

      Element messageML = context.getMessageML();
      Element form = messageML.getChildren().get(0);
      Element button = form.getChildren().get(0);

      assertEquals("Button class", Button.class, button.getClass());
      if("primary-destructive".equals(clazz)) {
        clazz = "primary";
      }
      if("secondary-destructive".equals(clazz)) {
        clazz = "secondary";
      }
      verifyButtonPresentation((Button) button, null, type, clazz, innerText);
    }
  }

  @Test
  public void testValidateButtonNewClasses() throws Exception {
    String input = "<messageML><form id=\"test\">"
        + "<button name=\"send-answers\" type=\"action\" class=\"primary\">Send Answers</button>\n"
        + "<button name=\"send-answers\" type=\"action\" class=\"secondary\">Send Answers</button>\n"
        + "<button name=\"send-answers\" type=\"action\" class=\"tertiary\">Send Answers</button>\n"
        + "<button name=\"send-answers\" type=\"action\" class=\"destructive\">Send Answers</button>"
        + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION); // if no exception thrown all is ok
  }

  @Test
  public void testTypelessButtonWithoutName() {
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
  public void testActionButtonWithoutName() {
    String innerText = "Typeless Button Without Name";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + ACTION_TYPE + "\">" + innerText + "</button></form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on action button without name");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Attribute \"name\" is required for action buttons", e.getMessage());
    }
  }

  @Test
  public void testActionButtonWithBlankName() {
    String innerText = "Typeless Button Without Name";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + ACTION_TYPE + "\" name=\" \">" + innerText + "</button></form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on action button without name");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Attribute \"name\" is required for action buttons", e.getMessage());
    }
  }

  @Test
  public void testResetButtonWithName() {
    String innerText = "Reset";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + RESET_TYPE + "\" name=\" \">" + innerText + "</button></form></messageML>";

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
    String name = "btn-name";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + ACTION_TYPE + "\" name=\"" + name + "\"></button></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The \"button\" element must have at least one child that is any of the following elements: [text content].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testMisplacedButton() {
    String innerText = "Misplaced Button";
    String input = "<messageML><button type=\"" + RESET_TYPE + "\">" + innerText + "</button></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on button out of a form tag");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Element \"button\" can only be a inner child of the following elements: [form, uiaction]", e.getMessage());
    }
  }

  @Test
  public void testBadTypeButton() {
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
  public void testBadClassButton() {
    String innerText = "Invalid Class Button";
    String clazz = "outclassed";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + RESET_TYPE + "\" class=\"" + clazz + "\">" + innerText
            + "</button></form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on invalid class button");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Attribute \"class\" must be \"primary\", \"secondary\", "
          + "\"tertiary\" or \"destructive\" (\"primary-destructive\" and "
          + "\"secondary-destructive\" are deprecated)", e.getMessage());
    }
  }

  @Test
  public void testBadAttributeButton() {
    String innerText = "Invalid Attribute Button";
    String invalidAttribute = "invalid-attribute";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><button type=\"" + RESET_TYPE + "\" " + invalidAttribute + "=\"invalid\">" + innerText
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
    assertNull("Button clazz attribute", button.getAttribute(CLASS_ATTR));
    assertEquals("Button inner text", "SELECT", button.getChild(0).asText());

    assertEquals("Button markdown", "Form (log into desktop client to answer):\n---\n(Button:SELECT)\n\n\n---\n", context.getMarkdown());
    assertEquals("Button presentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"example\"><div><button type=\"action\" name=\"div-button\">SELECT</button></div></form></div>",
        context.getPresentationML());
  }

  @Test
  public void testButtonInUIAction() throws Exception {
    String input = "<messageML><ui-action action=\"open-im\" user-ids=\"[123]\">" +
            "<button>Open by stream ID</button>" +
            "</ui-action></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element uiAction = messageML.getChildren().get(0);
    Element button = uiAction.getChildren().get(0);

    assertEquals("Button class", Button.class, button.getClass());
    assertTrue(button.getAttributes().isEmpty());
  }

  @Test
  public void testButtonWithAttributesInUIAction() throws Exception {
    String input = "<messageML><ui-action action=\"open-im\" user-ids=\"[123]\">" +
            "<button class=\"primary\" title=\"This is a UIAction button\">Open by stream ID</button>" +
            "</ui-action></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element uiAction = messageML.getChildren().get(0);
    Element button = uiAction.getChildren().get(0);

    assertEquals("Button class", Button.class, button.getClass());
    assertEquals(2, button.getAttributes().size());
  }

  @Test
  public void testButtonWithInvalidAttributesInUIAction() throws Exception {
    String input = "<messageML><ui-action action=\"open-im\" user-ids=\"[123]\">" +
            "<button type=\"reset\">Open by stream ID</button>" +
            "</ui-action></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attributes \"type\" and \"name\" are not allowed on a button inside a UIAction.");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testBiContextButton() throws InvalidInputException, IOException, ProcessingException {
    MessageMLContext messageMLContext = new MessageMLContext(null);
    String input = "<messageML>"
        + "<form id=\"all-elements\">"
        + "<button name=\"name01\" class=\"primary\" type=\"action\">Button Text</button>"
        + "</form>"
        + "</messageML>";

    messageMLContext.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    List<BiItem> items = messageMLContext.getBiContext().getItems();

    Map<String, Object> buttonExpectedAttributes = Stream.of(new String[][] {
        {BiFields.STYLE_COLOR.getValue(), "primary"},
        {BiFields.TYPE.getValue(), "action"},
    }).collect(Collectors.toMap(property -> property[0], property -> property[1]));

    BiItem buttonBiItemExpected = new BiItem(BiFields.BUTTON.getValue(), buttonExpectedAttributes);

    assertEquals(2, items.size());
    assertSameBiItem(buttonBiItemExpected, items.get(0));
    assertMessageLengthBiItem(items.get(1), input.length());
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
          + innerText + "</button>" + ACTION_BTN_ELEMENT + "</form></div>";
    } else {
      return "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR
          + "\"><button type=\"" + type + "\"" + getClassPresentationML(clazz) + getNamePresentationML(name) + ">"
          + innerText + "</button></form></div>";
    }
  }

  private String getExpectedButtonMarkdown(String innerText, Boolean shouldHaveAdditionalStandardActionBtn) {
    if (shouldHaveAdditionalStandardActionBtn) {
      return "Form (log into desktop client to answer):\n---\n(Button:"+ innerText + ")" + ACTION_BTN_MARKDOWN + "\n---\n";
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
