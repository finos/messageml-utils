package org.symphonyoss.symphony.messageml.elements.form;

import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.elements.Checkbox;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
  public void testPresentationMLCheckBoxWithLinebreaksAndWhitespacesBetweenTags() throws Exception {
    String input = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"checkbox-group\">\n" +
        "<input type=\"checkbox\" name=\"%s\" value=\"%s\"/>\n" +
        " <label>%s</label>\n" +
        "</div>" + ACTION_BTN_ELEMENT +
        "</form></div>", this.name, this.value, this.text);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, value, text, null, false);
    verifyCheckboxMarkdown(context, text);
  }

  @Test
  public void testPresentationMLCheckbox() throws Exception {
    String input = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"checkbox-group\">" +
        "<input type=\"checkbox\" name=\"%s\" value=\"%s\"/>" +
        "<label>%s</label>" +
        "</div>" + ACTION_BTN_ELEMENT + "</form></div>", this.name, this.value, this.text);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, value, text, null, false);
    verifyCheckboxMarkdown(context, text);
  }

  @Test
  public void testPresentationMLCheckboxWithOnlyNameAttribute() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"checkbox-form\">" +
        "<input type=\"checkbox\" name=\"checkbox-name\"/>" + ACTION_BTN_ELEMENT +
        "</form></div>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    verifyMessageMLObjectsForCheckbox(context);
    String presentationML = context.getPresentationML();
    String expectedPresentationML =
        String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"checkbox-form\"><input type=\"checkbox\" name=\"checkbox-name\" value=\"on\"/>" +
                "<button type=\"action\" name=\"actionName\">Send</button></form></div>", RadioTest.getInputId(presentationML));
    assertEquals(expectedPresentationML, presentationML);

    verifyCheckboxMarkdown(context, null);
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
  public void testLabelAttrPresentationMLCheckbox() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"checkbox-group\">" +
        "<input dummy=\"test\" id=\"id1\" type=\"checkbox\" name=\"name2\" value=\"value1\"/>" +
        "<label>Text 1</label>" +
        "</div></form></div>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"dummy\" is not allowed in \"checkbox\"");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testLabelMessageMLCheckbox() throws Exception {
    String input = "<messageML>\n"
        + "   <form id=\"example\">\n"
        + "      <checkbox name=\"fruits\" value=\"orange\">Orange</checkbox> \n"
        + "      <button type=\"action\" name=\"send-answers\">Submit</button>\n"
        + "   </form>\n"
        + "</messageML>";
    context.parseMessageML(input, "", MessageML.MESSAGEML_VERSION);
    String presentationML = context.getPresentationML();
    int startId = presentationML.indexOf("label for=\"");
    int endId = presentationML.indexOf('"', startId + "label for=\"".length());
    String id = presentationML.substring(startId + "label for=\"".length(), endId);
    assertTrue(id.startsWith("checkbox-group-"));

    String expectedResult = String.format(
        "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "    <form id=\"example\">"
        + "       <div class=\"checkbox-group\"><input type=\"checkbox\" name=\"fruits\" value=\"orange\" id=\"%s\"/><label "
        + "for=\"%s\">Orange</label></div>"
        + "        <button type=\"action\" name=\"send-answers\">Submit</button>"
        + "    </form>"
        + " </div>", id, id);
    assertEquals(expectedResult, presentationML);
  }

  @Test
  public void testLabelWithUnderscoreMessageMLCheckbox() throws Exception {
    String input = "<messageML>\n"
            + "   <form id=\"example\">\n"
            + "      <checkbox name=\"fruits\" value=\"orange\">Orange_fruit</checkbox> \n"
            + "      <button type=\"action\" name=\"send-answers\">Send</button>\n"
            + "   </form>\n"
            + "</messageML>";
    context.parseMessageML(input, "", MessageML.MESSAGEML_VERSION);
    String presentationML = context.getPresentationML();
    int startId = presentationML.indexOf("label for=\"");
    int endId = presentationML.indexOf('"', startId + "label for=\"".length());
    String id = presentationML.substring(startId + "label for=\"".length(), endId);
    assertTrue(id.startsWith("checkbox-group-"));

    String expectedResult = String.format(
            "<div data-format=\"PresentationML\" data-version=\"2.0\">"
                    + "    <form id=\"example\">"
                    + "       <div class=\"checkbox-group\"><input type=\"checkbox\" name=\"fruits\" value=\"orange\" id=\"%s\"/><label "
                    + "for=\"%s\">Orange_fruit</label></div>"
                    + "        <button type=\"action\" name=\"send-answers\">Send</button>"
                    + "    </form>"
                    + " </div>", id, id);
    assertEquals(expectedResult, presentationML);
    String expectedMarkdown = "    Form (log into desktop client to answer):\n" +
            "---\n" +
            "       (Checkbox:Orange\\_fruit)        (Button:Send)    \n" +
            "---\n" +
            " ";
    assertEquals(expectedMarkdown, context.getMarkdown());
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
    verifyCheckboxMarkdown(context, text);
  }

  @Test
  public void testNonCheckedCompleteCheckbox() throws Exception {
    String input = buildMessageMLFromParameters(name, value, text, checked, true);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, value, text, checked, true);
    verifyCheckboxMarkdown(context, text);
  }

  @Test
  public void testNoCheckedParameterCheckbox() throws Exception {
    String input = buildMessageMLFromParameters(name, value, text, checked, false);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, value, text, checked, false);
    verifyCheckboxMarkdown(context, text);
  }

  @Test
  public void testNoValueParameterCheckbox() throws Exception {
    String input = buildMessageMLFromParameters(name, null, text, checked, true);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, null, text, checked, true);
    verifyCheckboxMarkdown(context, text);
  }

  @Test
  public void testSimplerCheckbox() throws Exception {
    String input = buildMessageMLFromParameters(name, null, text, checked, false);
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForCheckbox(context);
    verifyCheckboxPresentationML(context, name, null, text, checked, false);
    verifyCheckboxMarkdown(context, text);
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
        String.format("<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"checkbox-form\"><input type=\"checkbox\" name=\"checkbox-name\" checked=\"false\" value=\"checkbox-value\"/>" +
                "<button type=\"action\" name=\"actionName\">Send</button></form></div>", RadioTest.getInputId(presentationML));
    assertEquals(expectedPresentationML, presentationML);
    verifyCheckboxMarkdown(context, null);
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
  public void testMoreThanFiftyCheckboxesWithinForm() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\"><div>" +
            StringUtils.repeat("<checkbox name=\"grp\" value=\"chk\">Item</checkbox>", 51) +
            "</div></form></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"form\" cannot have more than 50 children of the following elements: [checkbox, radio].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testMoreThanFiftyCheckboxesAsTableSelectWithinForm() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\"><table>" +
            "<thead><tr><td>H1</td><td>H2</td><td>H3</td><td><input type=\"checkbox\" name=\"tablesel-header\" value=\"on\"/></td></tr></thead>" +
            "<tbody>" +
            StringUtils.repeat("<tr><td>A</td><td>B</td><td>C</td><td><input type=\"checkbox\" name=\"tablesel-row\" value=\"on\"/></td></tr>",51) +
            "</tbody>" +
            "<tfoot><tr><td>F1</td><td>F2</td><td>F3</td><td><input type=\"checkbox\" name=\"tablesel-footer\" value=\"on\"/></td></tr></tfoot>" +
            "</table></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"form\" cannot have more than 50 children of the following elements: [checkbox, radio].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  private static Stream<Arguments> messageMlStream() {
    return Stream.of(
        Arguments.of(
            "<checkbox name=\"id1\">Red</checkbox>"
                + "<checkbox name=\"id2\" value=\"value02\" checked=\"false\">Green</checkbox>"
                + "<checkbox name=\"id3\" value=\"value03\" label=\"labelOne\" checked=\"true\">Red</checkbox>"
                + "<checkbox name=\"id4\" value=\"value04\" checked=\"false\">Yellow</checkbox>"
                + "<checkbox name=\"id5\" value=\"value02\" label=\"labelTwo\" checked=\"true\">Blue</checkbox>",
            Stream.of(new Object[][] {
                {BiFields.LABEL.getValue(), 2},
                {BiFields.OPTIONS_COUNT.getValue(), 5},
                {BiFields.DEFAULT.getValue(), 1},
            }).collect(Collectors.toMap(property -> property[0], property -> property[1]))),

        Arguments.of(
            "<checkbox name=\"id1\">Red</checkbox>"
                + "<checkbox name=\"id2\" value=\"value02\">Green</checkbox>"
                + "<checkbox name=\"id3\" value=\"value03\" label=\"labelOne\">Red</checkbox>"
                + "<checkbox name=\"id4\" value=\"value04\" checked=\"false\">Yellow</checkbox>"
                + "<checkbox name=\"id5\" value=\"value02\" label=\"labelTwo\">Blue</checkbox>",
            Stream.of(new Object[][] {
                {BiFields.LABEL.getValue(), 2},
                {BiFields.OPTIONS_COUNT.getValue(), 5},
            }).collect(Collectors.toMap(property -> property[0], property -> property[1])))
    );
  }

  @ParameterizedTest
  @MethodSource("messageMlStream")
  void testBiContextCheckBox(String checkBoxML, Map<String, Object> expectedAttributes)
      throws InvalidInputException, IOException, ProcessingException {
    MessageMLContext messageMLContext = new MessageMLContext(null);

    String input = String.format(
        "<messageML>\n "
            + "<form id=\"form_id\">\n "
            + "%s\n"
            + "<button name=\"name01\">Submit</button>"
            + "</form>\n </messageML>",
        checkBoxML);

    messageMLContext.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    List<BiItem> items = messageMLContext.getBiContext().getItems();

    BiItem checkBoxBiItemExpected = new BiItem(BiFields.CHECKBOX.getValue(), expectedAttributes);

    Assertions.assertEquals(4, items.size());
    Assertions.assertEquals(BiFields.CHECKBOX.getValue(), items.get(0).getName());
    assertSameBiItem(checkBoxBiItemExpected, items.get(0));
    assertMessageLengthBiItem(items.get(3), input.length());
  }

  private String buildMessageMLFromParameters(String name, String value, String text, String checked, boolean shouldSendCheckedAttribute) {
    return "<messageML><form id=\"" + formId + "\"><checkbox" +
        (name != null ? String.format(" name=\"%s\"", name) : "") +
        (value != null ? String.format(" value=\"%s\"", value) : "") +
        (shouldSendCheckedAttribute ? String.format(" checked=\"%s\"", checked) : "") +
        ">" +
        (text != null ? text : "") +
        "</checkbox>" + ACTION_BTN_ELEMENT + "</form></messageML>";
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
    String id = RadioTest.getInputId(presentationML);
    String expectedPresentationML = buildExpectedPresentationMLForCheckbox(id, name, value, text, checked, shouldShowChecked);
    assertEquals(expectedPresentationML, presentationML);
  }

  private String buildExpectedPresentationMLForCheckbox(String id, String name, String value, String text, String checked, boolean shouldShowChecked) {
    return "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + formId + "\"><div class=\"checkbox-group\"><input type=\"checkbox\"" +
        String.format(" name=\"%s\"", name) +
        (shouldShowChecked ? String.format(" checked=\"%s\"", checked) : "") +
        (value != null ? String.format(" value=\"%s\"", value) : " value=\"on\"") +
        " id=\"" + id + "\"/><label for=\""+ id + "\">" +
        (text != null ? text : "") +
        "</label></div>" + ACTION_BTN_ELEMENT + "</form></div>";
  }

  private void verifyCheckboxMarkdown(MessageMLContext context, String label) {
    String markdown = context.getMarkdown();
    String expectedMarkdown  = buildExpectedMarkdownForCheckbox(label);
    assertEquals(expectedMarkdown, markdown);
  }

  private String buildExpectedMarkdownForCheckbox(String label) {
    String expectedMarkdownText = ((label != null) ? ":" + label : "") ;
    return String.format("Form (log into desktop client to answer):\n---\n(Checkbox%s)%s\n---\n", expectedMarkdownText,
        ACTION_BTN_MARKDOWN);
  }
}
