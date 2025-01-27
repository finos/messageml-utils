package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.elements.*;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class TimezonePickerTest extends ElementTest {

  private String formId;

  @Before
  public void beforeEach() {
    this.formId = "timezone-picker-form";
  }

  @Test
  public void testTimezonePickerBasic() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">" +
            "<timezone-picker " +
            "name=\"timezone-name\" " +
            "value=\"W-SU\" " +
            "placeholder=\"Please pick a timezone\" " +
            "disabled-timezone='[\"Etc/GMT-14\",\"Australia/Victoria\",\"Asia/Phnom_Penh\"]' " +
            "required=\"true\"/>" +
            ACTION_BTN_ELEMENT + "</form></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element timezonePicker = form.getChildren().get(0);

    String presentationML = context.getPresentationML();

    String EXPECTED_MARKDOWN = "\n"
        + "   \n"
            + "(Timezone Picker:[Please pick a timezone])(Button:Send)\n"
        + "   \n";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "<form id=\"" + formId + "\">" +
            "<div class=\"timezone-picker\" " +
            "data-name=\"timezone-name\" " +
            "data-value=\"W-SU\" " +
            "data-placeholder=\"Please pick a timezone\" " +
            "data-disabled-timezone='[\"Etc/GMT-14\",\"Australia/Victoria\",\"Asia/Phnom_Penh\"]' " +
            "data-required=\"true\"></div>" + ACTION_BTN_ELEMENT +
            "</form></div>";

    assertEquals(Form.class, form.getClass());
    assertEquals(TimezonePicker.class, timezonePicker.getClass());
    assertTrue("Text should be empty", timezonePicker.getChildren().isEmpty());
    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, presentationML);
  }

  @Test
  public void testTimezonePickerWithLabelAndTitle() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">" +
            "<timezone-picker " +
            "name=\"timezone-name\" " +
            "value=\"America/New_York\" " +
            "label=\"Field label\" " +
            "title=\"This is a hint\" " +
            "placeholder=\"Please pick a timezone\" " +
            "disabled-timezone='[\"America/Detroit\", \"America/Los_Angeles\"]' " +
            "required=\"true\" "  +
            "formnovalidate=\"true\"/>" +
            ACTION_BTN_ELEMENT + "</form></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element timezonePicker = form.getChildren().get(0);

    String presentationML = context.getPresentationML();
    String timePickerRegex = ".*(\"timezone-picker-(.*?)\").*";
    Pattern pattern = Pattern.compile(timePickerRegex);
    Matcher matcher = pattern.matcher(presentationML);
    String uniqueLabelId = matcher.matches() ? matcher.group(2) : null;

    String EXPECTED_MARKDOWN = "\n"
        + "   \n"
            + "(Timezone Picker:[Please pick a timezone][Field label][This is a hint])(Button:Send)\n"
        + "   \n";
    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "<form id=\"" + formId + "\">" +
            "<div class=\"timezone-picker-group\" data-generated=\"true\">" +
            "<label for=\"timezone-picker-%s\">Field label</label>" +
            "<span class=\"info-hint\" data-target-id=\"timezone-picker-%s\" data-title=\"This is a hint\"></span>" +
            "<div class=\"timezone-picker\" " +
            "data-name=\"timezone-name\" " +
            "data-value=\"America/New_York\" " +
            "data-placeholder=\"Please pick a timezone\" " +
            "data-disabled-timezone='[\"America/Detroit\",\"America/Los_Angeles\"]' " +
            "data-required=\"true\" " +
            "data-formnovalidate=\"true\" " +
            "id=\"timezone-picker-%s\"></div>" +
            "</div>%s</form></div>", uniqueLabelId, uniqueLabelId, uniqueLabelId, ACTION_BTN_ELEMENT);

    assertEquals(Form.class, form.getClass());
    assertEquals(TimezonePicker.class, timezonePicker.getClass());
    assertTrue("Text should be empty", timezonePicker.getChildren().isEmpty());
    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, presentationML);
  }

  @Test
  public void testTimezonePickerTitleNewLine() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">" +
            "<timezone-picker " +
            "name=\"timezone-name\" " +
            "title=\"This is a \\n timezone picker\"/>" +
            ACTION_BTN_ELEMENT + "</form></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();
    String timePickerRegex = ".*(\"timezone-picker-(.*?)\").*";
    Pattern pattern = Pattern.compile(timePickerRegex);
    Matcher matcher = pattern.matcher(presentationML);
    String uniqueLabelId = matcher.matches() ? matcher.group(2) : null;

    String EXPECTED_MARKDOWN = "\n"
        + "   \n"
            + "(Timezone Picker:[This is a \\n timezone picker])(Button:Send)\n"
        + "   \n";
    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "<form id=\"" + formId + "\">" +
            "<div class=\"timezone-picker-group\" data-generated=\"true\">" +
            "<span class=\"info-hint\" data-target-id=\"timezone-picker-%s\" " +
            "data-title=\"This is a \\n timezone picker\"></span>" +
            "<div class=\"timezone-picker\" data-name=\"timezone-name\" id=\"timezone-picker-%s\"></div>" +
            "</div>%s</form></div>", uniqueLabelId, uniqueLabelId, ACTION_BTN_ELEMENT);

    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, presentationML);
  }

  @Test
  public void testTimezonePickerEmptyValue() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">" +
            "<timezone-picker " +
            "name=\"timezone-name\" " +
            "value=\"\"/>" +
            ACTION_BTN_ELEMENT + "</form></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
            "<form id=\"" + formId + "\">" +
            "<div class=\"timezone-picker\" " +
            "data-name=\"timezone-name\" " +
            "data-value=\"\"></div>" + ACTION_BTN_ELEMENT +
            "</form></div>";

    assertEquals("PresentationML", expectedPresentationML, presentationML);
  }

  @Test
  public void testTimezonePickerWithoutRequiredName() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">" +
            "<timezone-picker " +
            "title=\"This is a \\n timezone picker\"/>" +
            ACTION_BTN_ELEMENT + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTimezonePickerWrongValueAttr() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">" +
            "<timezone-picker " +
            "name=\"timezone-name\" " +
            "value=\"America/Rome\"/>" +
            ACTION_BTN_ELEMENT + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"value\" contains an invalid timezone: America/Rome");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTimePickerWrongDisabledTimezone() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">" +
            "<timezone-picker " +
            "name=\"timezone-name\" " +
            "disabled-timezone='[\"America/Detroit\", \"America/Paris\"]'/>" +
            ACTION_BTN_ELEMENT + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"disabled-timezone\" contains an invalid timezone: America/Paris");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTimePickerDisabledTimezoneNotArray() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">" +
            "<timezone-picker " +
            "name=\"timezone-name\" " +
            "disabled-timezone='\"America/Detroit\", \"America/Los_Angeles\"'/>" +
            ACTION_BTN_ELEMENT + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Error parsing json in attribute \"disabled-timezone\". " +
            "It should contain an array of Strings");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testEmptyMarkdown() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">" +
            "<timezone-picker " +
            "name=\"timezone-name\"/>" +
            ACTION_BTN_ELEMENT + "</form></messageML>";
    String EXPECTED_MARKDOWN = "\n"
        + "   \n"
            + "(Timezone Picker)(Button:Send)\n"
        + "   \n";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
  }

  @Test
  public void testMarkdownWithUnderscore() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">" +
            "<timezone-picker " +
            "name=\"timezone-name\" " +
            "title=\"This is_a_hint\"/>" +
            ACTION_BTN_ELEMENT + "</form></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    String EXPECTED_MARKDOWN = "\n"
        + "   \n"
            + "(Timezone Picker:[This is\\_a\\_hint])(Button:Send)\n"
        + "   \n";

    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
  }

  private static Stream<Arguments> messageMlStream() {
    return Stream.of(
        Arguments.of(
            "<timezone-picker name=\"disabled\" label=\"label01\" title=\"title01\" "
                + "placeholder=\"placeholder01\" required=\"true\" value=\"America/Indiana/Vincennes\" "
                + "disabled-timezone='[\"America/Detroit\", \"America/Indiana/Marengo\", "
                + "\"America/Indiana/Petersburg\"]' />\n",
            Stream.of(new Object[][] {
                {BiFields.PLACEHOLDER.getValue(), 1},
                {BiFields.TITLE.getValue(), 1},
                {BiFields.LABEL.getValue(), 1},
                {BiFields.REQUIRED.getValue(), 1},
                {BiFields.DEFAULT.getValue(), 1},
                {BiFields.VALIDATION_OPTIONS.getValue(), 1},
                {BiFields.VALIDATION.getValue(), 1},
            }).collect(Collectors.toMap(property -> property[0], property -> property[1]))),

        Arguments.of(
            "<timezone-picker name=\"disabled\" label=\"label01\" title=\"title01\" "
                + "placeholder=\"placeholder01\"/>", Stream.of(new Object[][] {
                {BiFields.PLACEHOLDER.getValue(), 1},
                {BiFields.TITLE.getValue(), 1},
                {BiFields.LABEL.getValue(), 1},
            }).collect(Collectors.toMap(property -> property[0], property -> property[1])))
    );
  }

  @ParameterizedTest
  @MethodSource("messageMlStream")
  void testBiContextTimeZonePicker_withValidation(String timeZoneML, Map<String, Object> expectedAttributes)
      throws InvalidInputException, IOException, ProcessingException {
    MessageMLContext messageMLContext = new MessageMLContext(null);
    String input = String.format(
        "<messageML>\n "
            + "<form id=\"form_id\">\n "
            + "%s\n"
            + "<button name=\"name\">Submit</button>\n "
            + "</form>\n </messageML>",
        timeZoneML);

    messageMLContext.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    List<BiItem> items = messageMLContext.getBiContext().getItems();

    BiItem timeZonePickerBiItemExpected = new BiItem(BiFields.TIMEZONE_PICKER.getValue(), expectedAttributes);

    assertEquals(4, items.size());
    assertEquals(BiFields.TIMEZONE_PICKER.getValue(), items.get(0).getName());
    assertSameBiItem(timeZonePickerBiItemExpected, items.get(0));
    assertMessageLengthBiItem(items.get(3), input.length());
  }
}
