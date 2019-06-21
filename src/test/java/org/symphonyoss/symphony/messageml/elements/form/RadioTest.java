package org.symphonyoss.symphony.messageml.elements.form;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.Radio;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class RadioTest extends ElementTest {
  private String formId;

  @Before
  public void beforeEach() {
    this.formId = "radio-form";
  }

  @Test
  public void testPresentationMLRadio() throws Exception {
    Set<RadioModel> radios = new HashSet<>();
    radios.add(new RadioModel("value01", "First"));
    radios.add(new RadioModel("value02", "Second"));
    radios.add(new RadioModel("value03", "Third"));

    String input = buildPresentationMLForRadio(radios, Boolean.FALSE);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForRadio(context);
    verifyRadioMarkdown(context, radios);
    verifyRadioPresentationML(context, radios);
  }

  @Test
  public void testInvalidPresentationMLRadio() throws Exception {
    String input = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"radio_name\" value=\"radio_value\"/>" +
        "<label>one</label><label>other</label>" +
        "</div></form></div>");

    assertException(input, InvalidInputException.class, "Invalid PresentationML for the \"radio\""
        + " element");
  }

  @Test
  public void testInvalidAttrPresentationMLRadio() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input id=\"id1\" type=\"radio\" name=\"name2\" value=\"value1\"/>" +
        "<label>Text 1</label>" +
        "</div></form></div>";

    assertException(input, InvalidInputException.class, "Attribute \"id\" is not allowed in \"radio\"");
  }

  @Test
  public void testInvalidPresentationMLRadioTwoInputs() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<input type=\"radio\" name=\"name2\" value=\"value1\"/>" +
        "<input type=\"radio\" name=\"name2\" value=\"value2\"/>" +
        "</div></form></div>";

    assertException(input, InvalidInputException.class, "Invalid PresentationML for the \"radio\" element");
  }

  @Test
  public void testInvalidPresentationMLRadioTwoLabels() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "<form id=\"" + formId + "\">" +
        "<div class=\"radio-group\">" +
        "<label>Text 1</label>" +
        "<label>Text 2</label>" +
        "</div></form></div>";

    assertException(input, InvalidInputException.class, "Invalid PresentationML for the \"radio\" element");
  }

  @Test
  public void testCompleteFilledRadio() throws Exception {
    Set<RadioModel> radios = new HashSet<>();
    radios.add(new RadioModel("value01", "First", Boolean.TRUE));

    String input = buildMessageMLForRadio(radios);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForRadio(context);
    verifyRadioPresentationML(context, radios);
    verifyRadioMarkdown(context, radios);
  }

  @Test
  public void testNonCheckedCompleteRadio() throws Exception {
    Set<RadioModel> radios = new HashSet<>();
    radios.add(new RadioModel("value01", "First", Boolean.FALSE));

    String input = buildMessageMLForRadio(radios);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForRadio(context);
    verifyRadioPresentationML(context, radios);
    verifyRadioMarkdown(context, radios);
  }

  @Test
  public void testNoCheckedParameterRadio() throws Exception {
    Set<RadioModel> radios = new HashSet<>();
    radios.add(new RadioModel("value01", "First"));

    String input = buildMessageMLForRadio(radios);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForRadio(context);
    verifyRadioPresentationML(context, radios);
    verifyRadioMarkdown(context, radios);
  }

  @Test
  public void testNoValueParameterRadio() throws Exception {
    Set<RadioModel> radios = new HashSet<>();
    radios.add(new RadioModel(null, "First", Boolean.TRUE));

    String input = buildMessageMLForRadio(radios);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForRadio(context);
    verifyRadioPresentationML(context, radios);
    verifyRadioMarkdown(context, radios);
  }

  @Test
  public void testSimplerRadio() throws Exception {
    Set<RadioModel> radios = new HashSet<>();
    radios.add(new RadioModel(null, "First"));

    String input = buildMessageMLForRadio(radios);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    verifyMessageMLObjectsForRadio(context);
    verifyRadioPresentationML(context, radios);
    verifyRadioMarkdown(context, radios);
  }

  @Test
  public void testRadioWithoutName() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio value=\"value01\">First</radio>");
    input.append("</form></messageML>");

    assertException(input.toString(), InvalidInputException.class, "The attribute \"name\" is required");
  }

  @Test
  public void testRadioWithoutAny() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio></radio>");
    input.append("</form></messageML>");

    assertException(input.toString(), InvalidInputException.class, "The attribute \"name\" is required");
  }

  @Test
  public void testRadioWithNonTextContent() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\"><div>First</div></radio>");
    input.append("</form></messageML>");

    assertException(input.toString(), InvalidInputException.class, "Element \"div\" is not "
        + "allowed in \"radio\"");
  }

  @Test
  public void testNoTextParameterRadio() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\"></radio>");
    input.append("</form></messageML>");

    assertException(input.toString(), InvalidInputException.class, "The \"radio\" element must "
        + "have at least one child that is any of the following elements: [text content, bold, "
        + "italic].");
  }

  @Test
  public void testRadioWithInvalidValueForChecked() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\" checked=\"somethingElse\">First</radio>");
    input.append("</form></messageML>");

    assertException(input.toString(), InvalidInputException.class, "Attribute \"checked\" of element \"radio\" can only be "
        + "one of the following values: [true, false].");
  }

  @Test
  public void testRadioWithoutForm() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><radio name=\"groupId\">First</radio>");
    input.append("</messageML>");

    assertException(input.toString(), InvalidInputException.class, "Element \"radio\" can only be a child of the following "
        + "elements: [form]");
  }

  @Test
  public void testRadioWithInvalidAttribute() throws Exception {
    StringBuilder input = new StringBuilder("<messageML><form id=\"" + formId + "\">");
    input.append("<radio name=\"groupId\" invalid=\"true\">First</radio>");
    input.append("</form></messageML>");

    assertException(input.toString(), InvalidInputException.class, "Attribute \"invalid\" is not allowed in \"radio\"");
  }

  private void assertException(String input, Class<? extends Exception> exceptionClass, String exceptionMessage)
      throws InvalidInputException, IOException, ProcessingException {
    expectedException.expect(exceptionClass);
    expectedException.expectMessage(exceptionMessage);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  private void verifyMessageMLObjectsForRadio(MessageMLContext context) {
    MessageML messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element radio = form.getChildren().get(0);
    assertEquals(form.getClass(), Form.class);
    assertEquals(radio.getClass(), Radio.class);
  }

  private void verifyRadioMarkdown(MessageMLContext context, Set<RadioModel> radios) {
    String markdown = context.getMarkdown();
    String expectedMarkdown  = buildMarkdownForRadio(radios);
    assertEquals(expectedMarkdown, markdown);
  }

  private String buildMarkdownForRadio(Set<RadioModel> radios) {
    StringBuilder markDown = new StringBuilder("Form (log into desktop client to answer):\n---");

    for (RadioModel radio: radios) {
      markDown.append("\n(Radio Button:");
      markDown.append(radio.getLabel());
      markDown.append(")");
    }
    markDown.append("\n\n---\n");


    return markDown.toString();
  }

  private void verifyRadioPresentationML(MessageMLContext context, Set<RadioModel> radios) {
    String presentationML = context.getPresentationML();
    String expectedPresentationML = buildPresentationMLForRadio(radios, Boolean.TRUE);
    assertEquals(expectedPresentationML, presentationML);
  }

  private String buildMessageMLForRadio(Set<RadioModel> radios) {
    StringBuilder presentationML = new StringBuilder("<messageML><form id=\"" + formId + "\">");

    for (RadioModel radio: radios) {
      presentationML.append("<radio name=\"groupId\"");

      if(radio.getValue() != null) {
        presentationML.append(" value=\"").append(radio.getValue()).append("\"");
      }

      if (radio.isChecked()) {
        presentationML.append(" checked=\"true\"");
      }

      presentationML.append(">");

      if(radio.getLabel() != null) {
        presentationML.append(radio.getLabel());
      }
      presentationML.append("</radio>");
    }

    presentationML.append("</form></messageML>");

    return presentationML.toString();
  }

  private String buildPresentationMLForRadio(Set<RadioModel> radios, Boolean expected) {
    StringBuilder presentationML = new StringBuilder("<div data-format=\"PresentationML\" data-version=\"2.0\">");
    presentationML.append("<form id=\"" + formId + "\">");

    for (RadioModel radio: radios) {
      presentationML.append("<div class=\"radio-group\">");

      presentationML.append("<input type=\"radio\" name=\"groupId\" value=\"");

      if (expected && radio.getValue() == null) {
        presentationML.append("on");
      } else {
        presentationML.append(radio.getValue());
      }

      if (radio.isChecked()) {
        presentationML.append("\" checked=\"true");
      }

      presentationML.append("\"/>");

      presentationML.append("<label>");
      presentationML.append(radio.label);
      presentationML.append("</label>");
      presentationML.append("</div>");
    }

    presentationML.append("</form></div>");

    return presentationML.toString();
  }

  private class RadioModel {
    private String value;

    private String label;

    private Boolean checked;

    public RadioModel(String value, String label, Boolean checked) {
      this.value = value;
      this.label = label;
      this.checked = checked;
    }

    public RadioModel(String value, String label) {
      this(value, label, Boolean.FALSE);
    }

    public String getValue() {
      return this.value;
    }

    public String getLabel() {
      return this.label;
    }

    public Boolean isChecked() {
      return this.checked;
    }
  }
}