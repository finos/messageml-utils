package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.Option;
import org.symphonyoss.symphony.messageml.elements.Select;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class SelectOptionTest extends ElementTest {

  private static final String NAME_ATTR = "name";
  private static final String REQUIRED_ATTR = "required";
  private static final String DATA_PLACEHOLDER_ATTR = "data-placeholder";
  private static final String VALUE_ATTR = "value";
  private static final String SELECTED_ATTR = "selected";
  private static final String FORM_ID_ATTR = "text-field-form";

  @Test
  public void testCompleteRequiredSelect() throws Exception {
    String name = "complete-required-id";
    boolean required = true;
    String placeholder = "placeholder-here";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select data-placeholder=\""+placeholder+"\" name=\"" + name + "\" required=\"" + required +
            "\"><option value=\"\">Option 1</option></select>" + ACTION_BTN_ELE + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, name, true, required, placeholder);
  }

  @Test
  public void testCompleteNotRequiredSelect() throws Exception {
    String name = "complete-id";
    boolean required = false;
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\" required=\"" + required +
            "\"><option value=\"\">Option 1</option></select>" + ACTION_BTN_ELE + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, name, true, required, null);
  }

  @Test
  public void testSimpleSelect() throws Exception {
    String name = "simple-id";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"><option value=\"\">Option 1</option></select>" + ACTION_BTN_ELE + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, name, false,false, null);
  }

  @Test
  public void testDoubleOptionSelect() throws Exception {
    String name = "simple-id";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"><option value=\"1\">Option 1</option><option value=\"2\">" +
            "Option 2</option></select>" + ACTION_BTN_ELE + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, name, false,false, null);
  }

  @Test
  public void testOptionWithSelectedAttr() throws Exception {
    String name = "simple-id";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"><option selected=\"true\" value=\"1\">Option 1</option><option value=\"2\">" +
        "Option 2</option></select>" + ACTION_BTN_ELE + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, name, false,false, null);
  }

  @Test
  public void testDoubleOptionWithSelectedAttrAsTrue() throws Exception {
    String name = "simple-id";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"><option value=\"1\" selected=\"true\">Option 1</option><option selected=\"true\" value=\"2\">" +
        "Option 2</option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"select\" can only have one selected \"option\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testOptionWithInvalidValueForSelectedAttr() throws Exception {
    String name = "simple-id";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"><option value=\"1\" selected=\"something\">Option 1</option><option selected=\"true\" value=\"2\">" +
        "Option 2</option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"selected\" of element \"option\" can only be one of the following values: [true, false].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testChildlessSelect() throws Exception {
    String name = "childless-select";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The \"select\" element must have at least one child that is any of the following elements: [option].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectWithEmptyChild() throws Exception {
    String name = "empty-child-select";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"> </select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The \"select\" element must have at least one child that is any of the following elements: [option].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectWithoutName() throws Exception {
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select><option value=\"\">Option 1</option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectWithBlankName() throws Exception {
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\" \"><option value=\"\">Option 1</option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testOptionWithoutValue() throws Exception {
    String name = "nameless-option";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name +"\"><option>Option 1</option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"value\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectWithInvalidRequiredAttribute() throws Exception {
    String name = "invalid-required-select";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\" required=\"potato\"><option value=\"\">Option 1</option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"required\" of element \"select\" can only be one of the following values: [true, false].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectWithInvalidAttribute() throws Exception {
    String name = "invalid-attribute-select";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"  invalid=\"attribute\"><option value=\"\">Option 1</option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"invalid\" is not allowed in \"select\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testOptionWithInvalidAttribute() throws Exception {
    String name = "invalid-attribute-option";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"><option value=\"\" invalid=\"attribute\">Option 1</option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"invalid\" is not allowed in \"option\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectOutOfForm() throws Exception {
    String name = "out-of-form-select";
    String input = "<messageML><select name=\"" + name + "\"><option value=\"\">Option 1</option></select></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"select\" can only be a inner child of the following elements: [form]");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testOptionOutOfSelect() throws Exception {
    String input = "<messageML><option value=\"\">Option 1</option></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"option\" can only be a child of the following elements: [select]");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSelectWithInvalidChild() throws Exception {
    String name = "invalid-child-select";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"><span></span></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"span\" is not allowed in \"select\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testOptionWithInvalidChild() throws Exception {
    String name = "invalid-child-option";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"><option value=\"\"><span></span></option></select></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"span\" is not allowed in \"option\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  private String getRequiredPresentationML(String required) {
    if(required != null) {
      if(required.equals("true") || required.equals("false")) {
        return String.format(" required=\"%s\"", required);
      }
    }

    return "";
  }

  private String getExpectedSelectMarkdown(Select select) {
    String FORM_MARKDOWN_HEADER = "Form (log into desktop client to answer):\n---\n";
    String FORM_MARKDOWN_FOOTER = "---\n";
    String selectMarkdown = "(Dropdown:" + select.getAttribute(NAME_ATTR) + "):\n";

    for (Element option : select.getChildren()) {
      if (option instanceof Option) {
        selectMarkdown = selectMarkdown + "-" + option.getChild(0).asText() + "\n";
      }
    }
    return FORM_MARKDOWN_HEADER + selectMarkdown + ACTION_BTN_MD + "\n" + FORM_MARKDOWN_FOOTER;
  }

  private String getExpectedSelectPresentation(Select select) {
    String selectOpeningTag =
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR
            + "\"><select " + getPlaceholderAttribute(select.getAttribute(DATA_PLACEHOLDER_ATTR))
            + "name=\"" + select.getAttribute(NAME_ATTR) + "\""
            + getRequiredPresentationML(select.getAttribute(REQUIRED_ATTR)) + ">";
    String selectClosingTag = "</select>";
    String formDivClosingTag = "</form></div>";
    String selectChildren = "";

    for (Element option : select.getChildren()) {
      if (option instanceof Option) {
        selectChildren = selectChildren + "<option" + getOptionSelectedExpectedText(option) + " value=\"" +
            option.getAttribute(VALUE_ATTR) + "\">" + option.getChild(0).asText() + "</option>";
      }
    }
    return selectOpeningTag + selectChildren + selectClosingTag + ACTION_BTN_ELE + formDivClosingTag;
  }

  private String getPlaceholderAttribute(String placeholder) {
    return placeholder != null ? "data-placeholder=\"" + placeholder + "\" " : "";
  }

  private String getOptionSelectedExpectedText(Element option) {
    return option.getAttribute(SELECTED_ATTR) != null ? " selected=\"" + option.getAttribute(SELECTED_ATTR) + "\"" : "";
  }

  private void verifySelectPresentation(Select select, String name, boolean requiredAttrProvided, boolean requiredValue, String placeholder) {
    assertEquals("Select name attribute", name, select.getAttribute(NAME_ATTR));
    if (requiredAttrProvided) {
      assertEquals("Select required attribute", String.valueOf(requiredValue), select.getAttribute(REQUIRED_ATTR));
    } else {
      assertNull("Select required attribute", select.getAttribute(REQUIRED_ATTR));
    }

    if (placeholder != null) {
      assertEquals("Select placeholder attribute", placeholder, select.getAttribute(DATA_PLACEHOLDER_ATTR));
    } else {
      assertNull("Select placeholder attribute", select.getAttribute(DATA_PLACEHOLDER_ATTR));
    }

    assertEquals("Select presentationML", getExpectedSelectPresentation(select), context.getPresentationML());
    assertEquals("Select markdown", getExpectedSelectMarkdown(select), context.getMarkdown());
  }
}
