package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Before;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.elements.Checkbox;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import static org.junit.Assert.assertEquals;

public class CheckboxTest extends ElementTest {
  private String formId;
  private String name;
  private String value;
  private String text;
  private String checked;

  @Before
  public void beforeEach() {
    this.formId = "checkbox-form";
    this.name = "checkbox-name";
    this.value = "checkbox-value";
    this.text = "Checkbox Text";
    this.checked = "false";
  }

  @Test
  public void testPresentationMLCheckbox() throws Exception {
    String input = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"checkbox-group\">" +
        "<input type=\"checkbox\" name=\"%s\" value=\"%s\"/>" +
        "<label>%s</label>" +
        "</div>" + ACTION_BTN_ELE + "</form></div>", this.name, this.value, this.text);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, value, text, null, false);
    verifyCheckboxMarkdown(context, name);
  }

  @Test
  public void testPresentationMLCheckboxWithOnlyNameAttribute() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"checkbox-form\">" +
        "<input type=\"checkbox\" name=\"checkbox-name\"/>" + ACTION_BTN_ELE +
        "</form></div>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    verifyMessageMLObjectsForCheckbox(context);
    String presentationML = context.getPresentationML();
    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"checkbox-form\"><input type=\"checkbox\" name=\"checkbox-name\" value=\"on\"/><button type=\"action\" name=\"actionName\">Send</button></form></div>";
    assertEquals(expectedPresentationML, presentationML);

    verifyCheckboxMarkdown(context, name);
  }

  @Test
  public void testInvalidPresentationMLCheckbox() throws Exception {
    String input = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"checkbox-group\">" +
        "<input type=\"checkbox\" name=\"%s\" value=\"%s\"/>" +
        "<label>%s</label><label>other</label>" +
        "</div></form></div>", this.name, this.value, this.text);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid PresentationML for the \"checkbox\" element");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testInvalidAttrPresentationMLCheckbox() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"checkbox-group\">" +
        "<input id=\"id1\" type=\"checkbox\" name=\"name2\" value=\"value1\"/>" +
        "<label>Text 1</label>" +
        "</div></form></div>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"id\" is not allowed in \"checkbox\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testInvalidPresentationMLCheckboxTwoInputs() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"checkbox-group\">" +
        "<input type=\"checkbox\" name=\"name2\" value=\"value1\"/>" +
        "<input type=\"checkbox\" name=\"name2\" value=\"value2\"/>" +
        "</div></form></div>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid PresentationML for the \"checkbox\" element");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testInvalidPresentationMLCheckboxTwoLabels() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"checkbox-group\">" +
        "<label>Text 1</label>" +
        "<label>Text 2</label>" +
        "</div></form></div>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid PresentationML for the \"checkbox\" element");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCompleteFilledCheckbox() throws Exception {
    checked = "true";
    String input = buildMessageMLFromParameters(name, value, text, checked, true);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, value, text, checked, true);
    verifyCheckboxMarkdown(context, name);
  }

  @Test
  public void testNonCheckedCompleteCheckbox() throws Exception {
    String input = buildMessageMLFromParameters(name, value, text, checked, true);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, value, text, checked, true);
    verifyCheckboxMarkdown(context, name);
  }

  @Test
  public void testNoCheckedParameterCheckbox() throws Exception {
    String input = buildMessageMLFromParameters(name, value, text, checked, false);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, value, text, checked, false);
    verifyCheckboxMarkdown(context, name);
  }

  @Test
  public void testNoValueParameterCheckbox() throws Exception {
    String input = buildMessageMLFromParameters(name, null, text, checked, true);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, null, text, checked, true);
    verifyCheckboxMarkdown(context, name);
  }

  @Test
  public void testSimplerCheckbox() throws Exception {
    String input = buildMessageMLFromParameters(name, null, text, checked, false);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, null, text, checked, false);
    verifyCheckboxMarkdown(context, name);
  }

  @Test
  public void testCheckboxWithoutName() throws Exception {
    String input = buildMessageMLFromParameters(null, value, text, checked, true);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCheckboxWithBlankName() throws Exception {
    String input = buildMessageMLFromParameters(" ", value, text, checked, true);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCheckboxWithoutAny() throws Exception {
    String input = buildMessageMLFromParameters(null, null, null, "false", false);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCheckboxWithNonTextContent() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\"><checkbox name=\"name\" value=\"value\"><div>Value</div></checkbox></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"div\" is not allowed in \"checkbox\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testNoTextParameterCheckbox() throws Exception {
    String input = buildMessageMLFromParameters(name, value, null, checked, true);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    verifyMessageMLObjectsForCheckbox(context);
    String presentationML = context.getPresentationML();
    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"checkbox-form\"><input type=\"checkbox\" name=\"checkbox-name\" checked=\"false\" value=\"checkbox-value\"/><button type=\"action\" name=\"actionName\">Send</button></form></div>";
    assertEquals(expectedPresentationML, presentationML);

    verifyCheckboxMarkdown(context, name);
  }

  @Test
  public void testCheckboxWithInvalidValueForChecked() throws Exception {
    String input = buildMessageMLFromParameters(name, value, text, "somethingElse", true);

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"checked\" of element \"checkbox\" can only be one of the following values: [true, false].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCheckboxWithoutForm() throws Exception {
    String input = "<messageML><checkbox value=\"value\">Value</checkbox></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"checkbox\" can only be a inner child of the following elements: [form]");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCheckboxWithInvalidAttribute() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\"><checkbox invalid=\"true\" value=\"value\">Value</checkbox></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"invalid\" is not allowed in \"checkbox\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testMoreThanTwentyCheckboxesWithinForm() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\"><div><checkbox name=\"grp1\" value=\"chk01\">Item 01</checkbox>\n"
        + "<checkbox name=\"grp1\" value=\"chk02\">Item 02</checkbox><checkbox name=\"grp1\" value=\"chk03\">Item 03</checkbox>\n"
        + "<checkbox name=\"grp1\" value=\"chk04\">Item 04</checkbox><checkbox name=\"grp1\" value=\"chk05\">Item 05</checkbox>\n"
        + "<checkbox name=\"grp1\" value=\"chk06\">Item 06</checkbox><checkbox name=\"grp1\" value=\"chk07\">Item 07</checkbox>\n"
        + "<checkbox name=\"grp1\" value=\"chk08\">Item 08</checkbox><checkbox name=\"grp1\" value=\"chk09\">Item 09</checkbox>\n"
        + "<checkbox name=\"grp1\" value=\"chk10\">Item 10</checkbox><checkbox name=\"grp2\" value=\"chk11\">Item 11</checkbox>\n"
        + "<checkbox name=\"grp2\" value=\"chk12\">Item 12</checkbox><checkbox name=\"grp2\" value=\"chk13\">Item 13</checkbox>\n"
        + "<checkbox name=\"grp2\" value=\"chk14\">Item 14</checkbox><checkbox name=\"grp2\" value=\"chk15\">Item 15</checkbox>\n"
        + "<checkbox name=\"grp3\" value=\"chk16\">Item 16</checkbox><checkbox name=\"grp3\" value=\"chk17\">Item 17</checkbox>\n"
        + "<checkbox name=\"grp3\" value=\"chk18\">Item 18</checkbox><checkbox name=\"grp3\" value=\"chk19\">Item 19</checkbox>\n"
        + "<checkbox name=\"grp3\" value=\"chk20\">Item 20</checkbox><checkbox name=\"grp3\" value=\"chk21\">Item 21</checkbox>"
        + "</div></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"form\" cannot have more than 20 children of the following elements: [checkbox, radio].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testMoreThanTwentyCheckboxesAsTableSelectWithinForm() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\"><table><thead><tr><td>H1</td><td>H2</td><td>H3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-header\" value=\"on\"/></td></tr></thead><tbody><tr><td>A1</td><td>B1</td><td>C1</td><td"
        + "><input type=\"checkbox\" name=\"tablesel-row-1\" value=\"on\"/></td></tr><tr><td>A2</td><td>B2</td><td>C2</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-2\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-3\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-4\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-5\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-6\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-7\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-8\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-9\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-10\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-11\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-12\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-13\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-14\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-15\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-16\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-17\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-18\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-19\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-20\" value=\"on\"/></td></tr><tr><td>A3</td><td>B3</td><td>C3</td><td><input "
        + "type=\"checkbox\" name=\"tablesel-row-21\" value=\"on\"/></td></tr></tbody><tfoot><tr><td>F1</td><td>F2</td><td>F3</td><td"
        + "><input type=\"checkbox\" name=\"tablesel-footer\" value=\"on\"/></td></tr></tfoot></table></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"form\" cannot have more than 20 children of the following elements: [checkbox, radio].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  private String buildMessageMLFromParameters(String name, String value, String text, String checked, boolean shouldSendCheckedAttribute) {
    return "<messageML><form id=\"" + formId + "\"><checkbox" +
        (name != null ? String.format(" name=\"%s\"", name) : "") +
        (value != null ? String.format(" value=\"%s\"", value) : "") +
        (shouldSendCheckedAttribute ? String.format(" checked=\"%s\"", checked) : "") +
        ">" +
        (text != null ? text : "") +
        "</checkbox>" + ACTION_BTN_ELE + "</form></messageML>";
  }

  private void verifyMessageMLObjectsForCheckbox(MessageMLContext context) {
    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element checkbox = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(checkbox.getClass(), Checkbox.class);
  }

  private void verifyCheckboxPresentationML(MessageMLContext context, String name, String value, String text, String checked, boolean shouldShowChecked) {
    String presentationML = context.getPresentationML();
    String expectedPresentationML = buildExpectedPresentationMLForCheckbox(name, value, text, checked, shouldShowChecked);
    assertEquals(expectedPresentationML, presentationML);
  }

  private String buildExpectedPresentationMLForCheckbox(String name, String value, String text, String checked, boolean shouldShowChecked) {
    return "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + formId + "\"><div class=\"checkbox-group\"><input type=\"checkbox\"" +
        String.format(" name=\"%s\"", name) +
        (shouldShowChecked ? String.format(" checked=\"%s\"", checked) : "") +
        (value != null ? String.format(" value=\"%s\"", value) : " value=\"on\"") +
        "/><label>" +
        (text != null ? text : "") +
        "</label></div>" + ACTION_BTN_ELE + "</form></div>";
  }

  private void verifyCheckboxMarkdown(MessageMLContext context, String name) {
    String markdown = context.getMarkdown();
    String expectedMarkdown  = buildExpectedMarkdownForCheckbox(name);
    assertEquals(expectedMarkdown, markdown);
  }

  private String buildExpectedMarkdownForCheckbox(String name) {
    return String.format("Form (log into desktop client to answer):\n---\n(Checkbox:%s)%s\n---\n", name, ACTION_BTN_MD);
  }
}
