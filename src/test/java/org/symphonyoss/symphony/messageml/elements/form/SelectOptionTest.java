package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.Option;
import org.symphonyoss.symphony.messageml.elements.Select;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class SelectOptionTest extends ElementTest {

  private static final String NAME_ATTR = "name";
  private static final String REQUIRED_ATTR = "required";
  private static final String DATA_PLACEHOLDER_ATTR = "data-placeholder";
  private static final String VALUE_ATTR = "value";
  private static final String SELECTED_ATTR = "selected";
  private static final String FORM_ID_ATTR = "text-field-form";
  private static final String LABEL_ATTR = "label";
  private static final String TITLE_ATTR = "title";

  @Test
  public void testCompleteRequiredSelect() throws Exception {
    String name = "complete-required-id";
    boolean required = true;
    String placeholder = "placeholder-here";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select data-placeholder=\""+placeholder+"\" name=\"" + name + "\" required=\"" + required +
            "\"><option value=\"\">Option 1</option></select>" + ACTION_BTN_ELEMENT + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, name, true, required, placeholder, false, false);
  }

  @Test
  public void testCompleteRequiredSelectWithLabelAndTooltip() throws Exception {
    String name = "complete-required-id";
    String label = "label";
    String title = "tooltip";
    boolean required = true;
    String placeholder = "placeholder-here";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select data-placeholder=\""+placeholder+"\" name=\"" + name + "\" required=\"" + required + "\"" +
        " label=\"" + label + "\" title=\"" + title +
        "\"><option value=\"\">Option 1</option></select>" + ACTION_BTN_ELEMENT + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, name, true, required, placeholder, true, true);
  }

  @Test
  public void testCompleteNotRequiredSelect() throws Exception {
    String name = "complete-id";
    boolean required = false;
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\" required=\"" + required +
            "\"><option value=\"\">Option 1</option></select>" + ACTION_BTN_ELEMENT + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, name, true, required, null, false, false);
  }

  @Test
  public void testSimpleSelect() throws Exception {
    String name = "simple-id";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"><option value=\"\">Option 1</option></select>" + ACTION_BTN_ELEMENT
        + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, name, false,false, null, false, false);
  }

  @Test
  public void testDoubleOptionSelect() throws Exception {
    String name = "simple-id";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"><option value=\"1\">Option 1</option><option value=\"2\">" +
            "Option 2</option></select>" + ACTION_BTN_ELEMENT + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, name, false,false, null, false, false);
  }

  @Test
  public void testOptionWithSelectedAttr() throws Exception {
    String name = "simple-id";
    String input = "<messageML><form id=\"" + FORM_ID_ATTR + "\"><select name=\"" + name + "\"><option selected=\"true\" value=\"1\">Option 1</option><option value=\"2\">" +
        "Option 2</option></select>" + ACTION_BTN_ELEMENT + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element select = form.getChildren().get(0);

    assertEquals("Select class", Select.class, select.getClass());
    verifySelectPresentation((Select) select, name, false,false, null, false, false);
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

  private String getExpectedSelectMarkdown(Select select, boolean hasLabel, boolean hasTitle) {
    String FORM_MARKDOWN_HEADER = "Form (log into desktop client to answer):\n---\n";
    String FORM_MARKDOWN_FOOTER = "---\n";
    
    StringBuilder expectedMarkdown = new StringBuilder(FORM_MARKDOWN_HEADER);
    expectedMarkdown.append("(Dropdown");
    expectedMarkdown.append((select.getAttribute(DATA_PLACEHOLDER_ATTR) != null || hasLabel || hasTitle) ? ":" : "");
    expectedMarkdown.append((select.getAttribute(DATA_PLACEHOLDER_ATTR) != null) ? "[" + select.getAttribute(DATA_PLACEHOLDER_ATTR) + "]" : "");
    expectedMarkdown.append(hasLabel ? "[" + select.getAttribute(LABEL_ATTR) + "]" : "");
    expectedMarkdown.append(hasTitle ? "[" + select.getAttribute(TITLE_ATTR) + "]" : "");
    expectedMarkdown.append("):\n");

    for (Element option : select.getChildren()) {
      if (option instanceof Option) {
        expectedMarkdown.append("-" + option.getChild(0).asText() + "\n");
      }
    }
    
    expectedMarkdown.append(ACTION_BTN_MARKDOWN + "\n" + FORM_MARKDOWN_FOOTER);
    return expectedMarkdown.toString();
  }

  private String getExpectedSelectPresentation(Select select, boolean hasLabel, boolean hasTitle, String uniqueLabelId) {
    String selectOpeningTag =
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + FORM_ID_ATTR
            + "\">"
            + ((hasLabel || hasTitle) ? "<div class=\"dropdown-group\" data-generated=\"true\">" : "")
            + (hasLabel ? "<label for=\"dropdown-" + uniqueLabelId + "\">"+ select.getAttribute(LABEL_ATTR) +"</label>" : "")
            + (hasTitle ? "<span class=\"info-hint\" data-target-id=\"dropdown-" + uniqueLabelId + "\" data-title=\"" + select.getAttribute(TITLE_ATTR) + "\"></span>" : "")
            + "<select " + getPlaceholderAttribute(select.getAttribute(DATA_PLACEHOLDER_ATTR))
            + "name=\"" + select.getAttribute(NAME_ATTR) + "\""
            + getRequiredPresentationML(select.getAttribute(REQUIRED_ATTR)) 
            + ((hasLabel || hasTitle) ? " id=\"dropdown-" + uniqueLabelId + "\"" : "")
            + ">";
    String selectClosingTag = "</select>";
    String formDivClosingTag = "</form></div>";
    String selectChildren = "";

    for (Element option : select.getChildren()) {
      if (option instanceof Option) {
        selectChildren = selectChildren + "<option" + getOptionSelectedExpectedText(option) + " value=\"" +
            option.getAttribute(VALUE_ATTR) + "\">" + option.getChild(0).asText() + "</option>";
      }
    }
    return selectOpeningTag + selectChildren + selectClosingTag + ((hasLabel || hasTitle) ? "</div>" : "") + ACTION_BTN_ELEMENT + formDivClosingTag;
  }

  private String getPlaceholderAttribute(String placeholder) {
    return placeholder != null ? "data-placeholder=\"" + placeholder + "\" " : "";
  }

  private String getOptionSelectedExpectedText(Element option) {
    return option.getAttribute(SELECTED_ATTR) != null ? " selected=\"" + option.getAttribute(SELECTED_ATTR) + "\"" : "";
  }

  private void verifySelectPresentation(Select select, String name, boolean requiredAttrProvided, boolean requiredValue, 
                                        String placeholder, boolean hasLabel, boolean hasTitle) {
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

    String presentationML = context.getPresentationML();
    String dropdownRegex = ".*(\"dropdown-(.*?)\").*";
    Pattern pattern = Pattern.compile(dropdownRegex);
    Matcher matcher = pattern.matcher(presentationML);
    
    assertEquals("Select presentationML", getExpectedSelectPresentation(select, hasLabel, hasTitle, matcher.matches() ? matcher.group(2) : null), presentationML);
    assertEquals("Select markdown", getExpectedSelectMarkdown(select, hasLabel, hasTitle), context.getMarkdown());
  }
}
