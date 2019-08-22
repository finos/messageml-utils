package org.symphonyoss.symphony.messageml.elements.form;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.Radio;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class RadioTest extends ElementTest {
  private String formId;

  @Before
  public void beforeEach() {
    this.formId = "radio-form";
  }

  @Test
  public void testPresentationMLRadio() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\" value=\"value01\">First</radio>");
    input.append("<radio name=\"groupId\" value=\"value02\">Second</radio>");
    input.append("<radio name=\"groupId\" value=\"value03\">Third</radio>");
    input.append(ACTION_BTN_ELE);
    input.append("</form></messageML>");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element radio = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(radio.getClass(), Radio.class);

    StringBuilder expectedMarkdown = new StringBuilder("Form (log into desktop client to answer):\n---");
    expectedMarkdown.append("\n(Radio Button:groupId)");
    expectedMarkdown.append("(Radio Button:groupId)");
    expectedMarkdown.append("(Radio Button:groupId)");
    expectedMarkdown.append(ACTION_BTN_MD);
    expectedMarkdown.append("\n---\n");

    String markdown = context.getMarkdown();

    assertEquals(expectedMarkdown.toString(), markdown);

    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"groupId\" value=\"value01\"/>" +
        "<label>First</label></div>" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"groupId\" value=\"value02\"/>" +
        "<label>Second</label></div>" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"groupId\" value=\"value03\"/>" +
        "<label>Third</label></div>" + ACTION_BTN_ELE +
        "</form></div>");

    String presentationML = context.getPresentationML();
    assertEquals(expectedPresentationML, presentationML);
  }

  @Test
  public void testPresentationMLCheckboxWithOnlyNameAttribute() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"radio-form\">" +
        "<input type=\"radio\" name=\"radio-name\"/>" + ACTION_BTN_ELE +
        "</form></div>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element radio = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(radio.getClass(), Radio.class);
    
    String presentationML = context.getPresentationML();
    String expectedPresentationML ="<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"radio-form\"><input type=\"radio\" name=\"radio-name\" value=\"on\"/>" + ACTION_BTN_ELE + "</form></div>";
    assertEquals(expectedPresentationML, presentationML);

    StringBuilder expectedMarkdown = new StringBuilder("Form (log into desktop client to answer):\n---");
    expectedMarkdown.append("\n(Radio Button:radio-name)");
    expectedMarkdown.append(ACTION_BTN_MD);
    expectedMarkdown.append("\n---\n");

    String markdown = context.getMarkdown();

    assertEquals(expectedMarkdown.toString(), markdown);
  }

  @Test
  public void testInvalidPresentationMLRadio() throws Exception {
    String input = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"radio_name\" value=\"radio_value\"/>" +
        "<label>one</label><label>other</label>" +
        "</div></form></div>");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid PresentationML for the \"radio\""
        + " element");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testInvalidAttrPresentationMLRadio() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input id=\"id1\" type=\"radio\" name=\"name2\" value=\"value1\"/>" +
        "<label>Text 1</label>" +
        "</div></form></div>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"id\" is not allowed in \"radio\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testInvalidPresentationMLRadioTwoInputs() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"name2\" value=\"value1\"/>" +
        "<input type=\"radio\" name=\"name2\" value=\"value2\"/>" +
        "</div></form></div>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid PresentationML for the \"radio\" element");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testInvalidPresentationMLRadioTwoLabels() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<label>Text 1</label>" +
        "<label>Text 2</label>" +
        "</div></form></div>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid PresentationML for the \"radio\" element");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCompleteFilledRadio() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\" value=\"value01\" checked=\"true\">First</radio>");
    input.append(ACTION_BTN_ELE);
    input.append("</form></messageML>");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element radio = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(radio.getClass(), Radio.class);

    StringBuilder expectedMarkdown = new StringBuilder("Form (log into desktop client to answer):\n---");
    expectedMarkdown.append("\n(Radio Button:groupId)");
    expectedMarkdown.append(ACTION_BTN_MD);
    expectedMarkdown.append("\n---\n");

    String markdown = context.getMarkdown();

    assertEquals(expectedMarkdown.toString(), markdown);

    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"groupId\" value=\"value01\" checked=\"true\"/>" +
        "<label>First</label></div>"  + ACTION_BTN_ELE +
        "</form></div>");

    String presentationML = context.getPresentationML();
    assertEquals(expectedPresentationML, presentationML);
  }

  @Test
  public void testNonCheckedCompleteRadio() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\" value=\"value01\" checked=\"false\">First</radio>");
    input.append(ACTION_BTN_ELE);
    input.append("</form></messageML>");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element radio = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(radio.getClass(), Radio.class);

    StringBuilder expectedMarkdown = new StringBuilder("Form (log into desktop client to answer):\n---");
    expectedMarkdown.append("\n(Radio Button:groupId)");
    expectedMarkdown.append(ACTION_BTN_MD);
    expectedMarkdown.append("\n---\n");

    String markdown = context.getMarkdown();

    assertEquals(expectedMarkdown.toString(), markdown);

    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"groupId\" value=\"value01\" checked=\"false\"/>" +
        "<label>First</label></div>" + ACTION_BTN_ELE +
        "</form></div>");

    String presentationML = context.getPresentationML();
    assertEquals(expectedPresentationML, presentationML);
  }

  @Test
  public void testNoCheckedParameterRadio() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\" value=\"value01\">First</radio>");
    input.append(ACTION_BTN_ELE);
    input.append("</form></messageML>");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element radio = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(radio.getClass(), Radio.class);

    StringBuilder expectedMarkdown = new StringBuilder("Form (log into desktop client to answer):\n---");
    expectedMarkdown.append("\n(Radio Button:groupId)");
    expectedMarkdown.append(ACTION_BTN_MD);
    expectedMarkdown.append("\n---\n");

    String markdown = context.getMarkdown();

    assertEquals(expectedMarkdown.toString(), markdown);

    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"groupId\" value=\"value01\"/>" +
        "<label>First</label></div>" + ACTION_BTN_ELE +
        "</form></div>");

    String presentationML = context.getPresentationML();
    assertEquals(expectedPresentationML, presentationML);
  }

  @Test
  public void testNoValueParameterRadio() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\" checked=\"true\">First</radio>");
    input.append(ACTION_BTN_ELE);
    input.append("</form></messageML>");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element radio = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(radio.getClass(), Radio.class);

    StringBuilder expectedMarkdown = new StringBuilder("Form (log into desktop client to answer):\n---");
    expectedMarkdown.append("\n(Radio Button:groupId)");
    expectedMarkdown.append(ACTION_BTN_MD);
    expectedMarkdown.append("\n---\n");

    String markdown = context.getMarkdown();

    assertEquals(expectedMarkdown.toString(), markdown);

    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"groupId\" value=\"on\" checked=\"true\"/>" +
        "<label>First</label></div>" + ACTION_BTN_ELE +
        "</form></div>");

    String presentationML = context.getPresentationML();
    assertEquals(expectedPresentationML, presentationML);
  }

  @Test
  public void testSimplerRadio() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\">First</radio>");
    input.append(ACTION_BTN_ELE);
    input.append("</form></messageML>");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element radio = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(radio.getClass(), Radio.class);

    StringBuilder expectedMarkdown = new StringBuilder("Form (log into desktop client to answer):\n---");
    expectedMarkdown.append("\n(Radio Button:groupId)");
    expectedMarkdown.append(ACTION_BTN_MD);
    expectedMarkdown.append("\n---\n");

    String markdown = context.getMarkdown();

    assertEquals(expectedMarkdown.toString(), markdown);

    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"groupId\" value=\"on\"/>" +
        "<label>First</label></div>" + ACTION_BTN_ELE +
        "</form></div>");

    String presentationML = context.getPresentationML();
    assertEquals(expectedPresentationML, presentationML);
  }

  @Test
  public void testRadioWithoutName() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio value=\"value01\">First</radio>");
    input.append("</form></messageML>");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testRadioWithBlankName() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio value=\"value01\" name=\" \">First</radio>");
    input.append("</form></messageML>");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testRadioWithoutAny() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio></radio>");
    input.append("</form></messageML>");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testRadioWithNonTextContent() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\"><div>First</div></radio>");
    input.append("</form></messageML>");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"div\" is not "
        + "allowed in \"radio\"");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testNoTextParameterRadio() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\"></radio>");
    input.append(ACTION_BTN_ELE);
    input.append("</form></messageML>");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);
    
    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element checkbox = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(checkbox.getClass(), Radio.class);
    String presentationML = context.getPresentationML();
    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<input type=\"radio\" name=\"groupId\" value=\"on\"/>" + ACTION_BTN_ELE +
        "</form></div>");    
    
    assertEquals(expectedPresentationML, presentationML);

    StringBuilder expectedMarkdown = new StringBuilder("Form (log into desktop client to answer):\n---");
    expectedMarkdown.append("\n(Radio Button:groupId)");
    expectedMarkdown.append(ACTION_BTN_MD);
    expectedMarkdown.append("\n---\n");

    String markdown = context.getMarkdown();
    assertEquals(expectedMarkdown.toString(), markdown);
  }

  @Test
  public void testRadioWithInvalidValueForChecked() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\" checked=\"somethingElse\">First</radio>");
    input.append("</form></messageML>");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"checked\" of element \"radio\" can only be "
        + "one of the following values: [true, false].");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testRadioWithoutForm() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><radio name=\"groupId\">First</radio>");
    input.append("</messageML>");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"radio\" can only be a inner child of the following "
        + "elements: [form]");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testRadioWithInvalidAttribute() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\" invalid=\"true\">First</radio>");
    input.append("</form></messageML>");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"invalid\" is not allowed in \"radio\"");

    context.parseMessageML(input.toString(), null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testMoreThanTwentyRadioButtonsWithinForm() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\"><div><radio name=\"grp1\" value=\"rad01\">Item 01</radio>\n"
        + "<radio name=\"grp1\" value=\"rad02\">Item 02</radio><radio name=\"grp1\" value=\"rad03\">Item 03</radio>\n"
        + "<radio name=\"grp1\" value=\"rad04\">Item 04</radio><radio name=\"grp1\" value=\"rad05\">Item 05</radio>\n"
        + "<radio name=\"grp1\" value=\"rad06\">Item 06</radio><radio name=\"grp1\" value=\"rad07\">Item 07</radio>\n"
        + "<radio name=\"grp1\" value=\"rad08\">Item 08</radio><radio name=\"grp1\" value=\"rad09\">Item 09</radio>\n"
        + "<radio name=\"grp2\" value=\"rad10\">Item 10</radio><radio name=\"grp2\" value=\"rad11\">Item 11</radio>\n"
        + "<radio name=\"grp2\" value=\"rad12\">Item 12</radio><radio name=\"grp2\" value=\"rad13\">Item 13</radio>\n"
        + "<radio name=\"grp2\" value=\"rad14\">Item 14</radio><radio name=\"grp2\" value=\"rad15\">Item 15</radio>\n"
        + "<radio name=\"grp3\" value=\"rad16\">Item 16</radio><radio name=\"grp3\" value=\"rad17\">Item 17</radio>\n"
        + "<radio name=\"grp3\" value=\"rad18\">Item 18</radio><radio name=\"grp3\" value=\"rad19\">Item 19</radio>\n"
        + "<radio name=\"grp3\" value=\"rad20\">Item 20</radio><radio name=\"grp3\" value=\"rad21\">Item 21</radio>"
        + "</div></form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"form\" cannot have more than 20 children of the following elements: [checkbox, radio].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

}
