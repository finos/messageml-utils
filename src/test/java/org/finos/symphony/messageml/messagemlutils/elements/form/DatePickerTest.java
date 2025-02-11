package org.finos.symphony.messageml.messagemlutils.elements.form;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.elements.DatePicker;
import org.finos.symphony.messageml.messagemlutils.elements.Element;
import org.finos.symphony.messageml.messagemlutils.elements.ElementTest;
import org.finos.symphony.messageml.messagemlutils.elements.Form;
import org.finos.symphony.messageml.messagemlutils.elements.MessageML;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.exceptions.ProcessingException;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author enrico.molino (17/11/2020)
 */
public class DatePickerTest extends ElementTest {

  private String formId;

  @Before
  public void beforeEach() {
    this.formId = "datepicker-form";
  }

  @Test
  public void testDatePickerBasic() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\", \"to\": \"2020-09-20\"}, "
        + "{\"daysOfWeek\": [0,1]}]'"
        + "      highlighted-date='[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
        + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]}]'"
        + "      required=\"true\"\n"
        + "      format=\"yyyy-MM-dd\"\n"
        + "      formnovalidate=\"true\""
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element datePicker = form.getChildren().get(0);

    String presentationML = context.getPresentationML();

    String EXPECTED_MARKDOWN = "\n"
        + "   \n"
        + "(Date Picker:[Please pick a date])(Button:Send)\n"
        + "   \n";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2"
        + ".0\"><form id=\"datepicker-form\"><input type=\"date\" name=\"date-travel\" value=\"2020-09-15\" "
        + "placeholder=\"Please pick a date\" min=\"2020-09-01\" max=\"2020-09-29\" required=\"true\" "
        + "data-disabled-date='[{\"type\":\"date\",\"day\":\"2020-09-23\"},{\"type\":\"range\","
        + "\"from\":\"2020-09-18\",\"to\":\"2020-09-20\"},{\"type\":\"weekdays\",\"daysOfWeek\":[0,1]}]' "
        + "data-highlighted-date='[{\"type\":\"date\",\"day\":\"2020-09-24\"},{\"type\":\"range\","
        + "\"from\":\"2020-09-26\",\"to\":\"2020-09-28\"},{\"type\":\"date\",\"day\":\"2020-09-03\"},"
        + "{\"type\":\"weekdays\",\"daysOfWeek\":[4,6]}]' "
        + "data-format=\"yyyy-MM-dd\" data-formnovalidate=\"true\"></input><button type=\"action\" "
        + "name=\"actionName\">Send</button></form></div>";

    assertEquals(Form.class, form.getClass());
    assertEquals(DatePicker.class, datePicker.getClass());
    assertTrue("Text should be empty", datePicker.getChildren().isEmpty());
    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, presentationML);
  }

  @Test
  public void testDatePickerWithLabelAndTooltip() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      title=\"This is \\n a hint\"\n"
        + "      label=\"Departure date\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\", \"to\": \"2020-09-20\"}, "
        + "{\"daysOfWeek\": [0,1]}]'"
        + "      highlighted-date='[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
        + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]}]'"
        + "      required=\"true\"\n"
        + "      format=\"yyyy-MM-dd\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element datePicker = form.getChildren().get(0);

    String presentationML = context.getPresentationML();
    String datePickerRegex = ".*(\"date-picker-(.*?)\").*";
    Pattern pattern = Pattern.compile(datePickerRegex);
    Matcher matcher = pattern.matcher(presentationML);
    String uniqueLabelId = matcher.matches() ? matcher.group(2) : null;

    String EXPECTED_MARKDOWN = "\n"
        + "   \n"
        + "(Date Picker:[Departure date][This is \\n a hint][Please pick a date])(Button:Send)\n"
        + "   \n";
    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" "
        + "data-version=\"2.0\"><form id=\"datepicker-form\"><div class=\"date-picker-group\" "
        + "data-generated=\"true\"><label for=\"date-picker-%s\">Departure "
        + "date</label><span class=\"info-hint\" data-target-id=\"date-picker-%s\" "
        + "data-title=\"This is \\n a hint\"></span><input type=\"date\" name=\"date-travel\" "
        + "value=\"2020-09-15\" placeholder=\"Please pick a date\" min=\"2020-09-01\" "
        + "max=\"2020-09-29\" required=\"true\" data-disabled-date='[{\"type\":\"date\",\"day\":\"2020-09-23\"},"
        + "{\"type\":\"range\",\"from\":\"2020-09-18\",\"to\":\"2020-09-20\"},{\"type\":\"weekdays\","
        + "\"daysOfWeek\":[0,1]}]' data-highlighted-date='[{\"type\":\"date\",\"day\":"
        + "\"2020-09-24\"},{\"type\":\"range\",\"from\":\"2020-09-26\",\"to\":\"2020-09-28\"},{\"type\":"
        + "\"date\",\"day\":\"2020-09-03\"},{\"type\":\"weekdays\",\"daysOfWeek\":[4,6]}]' "
        + "data-format=\"yyyy-MM-dd\" "
        + "id=\"date-picker-%s\"></input></div><button type=\"action\" "
        + "name=\"actionName\">Send</button></form></div>", uniqueLabelId, uniqueLabelId, uniqueLabelId, uniqueLabelId);

    assertEquals(Form.class, form.getClass());
    assertEquals(DatePicker.class, datePicker.getClass());
    assertTrue("Text should be empty", datePicker.getChildren().isEmpty());
    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, presentationML);
  }

  @Test
  public void testDatePickerWithUnderscores() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
            + "<date-picker \n"
            + "      name=\"date-travel\"\n"
            + "      value=\"2020-09-15\"\n"
            + "      title=\"This_is_a_hint\"\n"
            + "      label=\"Departure_date\"\n"
            + "      placeholder=\"Please_pick_a_date\"\n"
            + "      min=\"2020-09-01\"\n"
            + "      max=\"2020-09-29\"\n"
            + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\", \"to\": \"2020-09-20\"}, "
            + "{\"daysOfWeek\": [0,1]}]'"
            + "      highlighted-date='[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
            + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]}]'"
            + "      required=\"true\"\n"
            + "      format=\"yyyy-MM-dd\"\n"
            + "    />"
            + ACTION_BTN_ELEMENT + "</form></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element datePicker = form.getChildren().get(0);

    String presentationML = context.getPresentationML();
    String datePickerRegex = ".*(\"date-picker-(.*?)\").*";
    Pattern pattern = Pattern.compile(datePickerRegex);
    Matcher matcher = pattern.matcher(presentationML);
    String uniqueLabelId = matcher.matches() ? matcher.group(2) : null;

    String EXPECTED_MARKDOWN = "\n"
        + "   \n"
            + "(Date Picker:[Departure\\_date][This\\_is\\_a\\_hint][Please\\_pick\\_a\\_date])(Button:Send)\n"
        + "   \n";
    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" "
            + "data-version=\"2.0\"><form id=\"datepicker-form\"><div class=\"date-picker-group\" "
            + "data-generated=\"true\"><label for=\"date-picker-%s\">Departure_"
            + "date</label><span class=\"info-hint\" data-target-id=\"date-picker-%s\" "
            + "data-title=\"This_is_a_hint\"></span><input type=\"date\" name=\"date-travel\" "
            + "value=\"2020-09-15\" placeholder=\"Please_pick_a_date\" min=\"2020-09-01\" "
            + "max=\"2020-09-29\" required=\"true\" data-disabled-date='[{\"type\":\"date\",\"day\":\"2020-09-23\"},"
            + "{\"type\":\"range\",\"from\":\"2020-09-18\",\"to\":\"2020-09-20\"},{\"type\":\"weekdays\","
            + "\"daysOfWeek\":[0,1]}]' data-highlighted-date='[{\"type\":\"date\",\"day\":"
            + "\"2020-09-24\"},{\"type\":\"range\",\"from\":\"2020-09-26\",\"to\":\"2020-09-28\"},{\"type\":"
            + "\"date\",\"day\":\"2020-09-03\"},{\"type\":\"weekdays\",\"daysOfWeek\":[4,6]}]' "
            + "data-format=\"yyyy-MM-dd\" "
            + "id=\"date-picker-%s\"></input></div><button type=\"action\" "
            + "name=\"actionName\">Send</button></form></div>", uniqueLabelId, uniqueLabelId, uniqueLabelId, uniqueLabelId);

    assertEquals(Form.class, form.getClass());
    assertEquals(DatePicker.class, datePicker.getClass());
    assertTrue("Text should be empty", datePicker.getChildren().isEmpty());
    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
    assertEquals("PresentationML", expectedPresentationML, presentationML);
  }

  @Test
  public void sendMissingFieldOnMessageML() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      value=\"2020-09-15\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\", \"to\": \"2020-09-20\"}, "
        + "{\"daysOfWeek\": [0,1]}]'"
        + "      highlighted-date='[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
        + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]}]'"
        + "      required=\"true\"\n"
        + "      format=\"yyyy-MM-dd\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";

    expectedException.expect(InvalidInputException .class);
    expectedException.expectMessage("The attribute \"name\" is required");
    context.parseMessageML(input,null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendMalformedJsonOnMessageML() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\", \"to\": \"2020-09-20\"}, "
        + "{\"daysOfWeek\": [0,1]}]'"
        + "      highlighted-date='[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
        + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]'"
        + "      required=\"true\"\n"
        + "      format=\"yyyy-MM-dd\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";

    expectedException.expect(InvalidInputException .class);
    expectedException.expectMessage("Error parsing json in attribute \"highlighted-date\": "
        + "Unexpected end-of-input: expected close marker for Object (start marker at [Source: "
        + "(String)\"[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, {\"day\": "
        + "\"2020-09-03\"}, {\"daysOfWeek\": [4,6]\"; line: 1, column: 92])");
    context.parseMessageML(input,null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendWrongDateInJsonFormatOnMessageML() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\", \"to\": \"2020-09-20\"}, "
        + "{\"daysOfWeek\": [0,1]}]'"
        + "      highlighted-date='[{\"day\": \"2020-09\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
        + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]}]'"
        + "      required=\"true\"\n"
        + "      format=\"yyyy-MM-dd\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("\"2020-09\" is not a valid date in ISO_8601 format");
    context.parseMessageML(input,null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendWrongDateIntervalInJsonFormatOnMessageML() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\"}, "
        + "{\"daysOfWeek\": [0,1]}]'"
        + "      highlighted-date='[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
        + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]}]'"
        + "      required=\"true\"\n"
        + "      format=\"yyyy-MM-dd\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Date interval \'type\' is unknown or null");
    context.parseMessageML(input,null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendFieldTooLongOnMessageML() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel-----------------------------------------------------------------------\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      required=\"true\"\n"
        + "      format=\"yyyy-MM-dd\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" length is bigger than maximum allowed"
        + " [64]");
    context.parseMessageML(input,null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendUnsupportedDateFormat() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\", \"to\": \"2020-09-20\"}, "
        + "{\"daysOfWeek\": [0,1]}]'"
        + "      highlighted-date='[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
        + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]}]'"
        + "      required=\"true\"\n"
        + "      format=\"yyyy-MM-dd HH:mm:ss\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"format\" contains an unsupported date format, "
        + "only 'M', 'd' and 'y' are supported with a space or '.','-','/',':' as separator");
    context.parseMessageML(input,null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendSeparator1DateFormat() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\", \"to\": \"2020-09-20\"}, "
        + "{\"daysOfWeek\": [0,1]}]'"
        + "      highlighted-date='[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
        + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]}]'"
        + "      required=\"true\"\n"
        + "      format=\"yyyy/MM/dd\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";
    context.parseMessageML(input,null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendSeparator2DateFormat() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\", \"to\": \"2020-09-20\"}, "
        + "{\"daysOfWeek\": [0,1]}]'"
        + "      highlighted-date='[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
        + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]}]'"
        + "      required=\"true\"\n"
        + "      format=\"yyyy:MM:dd\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";
    context.parseMessageML(input,null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendSeparator3DateFormat() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\", \"to\": \"2020-09-20\"}, "
        + "{\"daysOfWeek\": [0,1]}]'"
        + "      highlighted-date='[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
        + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]}]'"
        + "      required=\"true\"\n"
        + "      format=\"yyyy.MM.dd\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";
    context.parseMessageML(input,null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void sendSeparator4DateFormat() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date='[{\"day\": \"2020-09-23\"}, {\"from\": \"2020-09-18\", \"to\": \"2020-09-20\"}, "
        + "{\"daysOfWeek\": [0,1]}]'"
        + "      highlighted-date='[{\"day\": \"2020-09-24\"}, {\"from\": \"2020-09-26\", \"to\": \"2020-09-28\"}, "
        + "{\"day\": \"2020-09-03\"}, {\"daysOfWeek\": [4,6]}]'"
        + "      required=\"true\"\n"
        + "      format=\"yyyy MM dd\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";
    context.parseMessageML(input,null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testEmptyMarkdown() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker name=\"date-travel\"/>"
        + ACTION_BTN_ELEMENT + "</form></messageML>";
    String EXPECTED_MARKDOWN = "\n"
        + "   \n"
        + "(Date Picker)(Button:Send)\n"
        + "   \n";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
  }

  @Test
  public void testPartialMarkdown() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker name=\"date-travel\" placeholder=\"Please pick a date\"/>"
        + ACTION_BTN_ELEMENT + "</form></messageML>";
    String EXPECTED_MARKDOWN = "\n"
        + "   \n"
        + "(Date Picker:[Please pick a date])(Button:Send)\n"
        + "   \n";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    assertEquals("Markdown", EXPECTED_MARKDOWN, context.getMarkdown());
  }

  private static Stream<Arguments> messageMlStream() {
    return Stream.of(
        Arguments.of(
            "<date-picker name=\"rules\" value=\"2020-08-28\" label=\"label01\" "
                + "title=\"title01\" min=\"2021-01-04\" max=\"2021-01-27\" "
                + "disabled-date='[{\"from\": \"2021-01-13\", \"to\": \"2021-01-15\"},"
                + " {\"from\": \"2021-01-19\", \"to\": \"2021-01-21\"}, {\"day\": \"2021-01-06\"}, "
                + "{\"daysOfWeek\": [0,6]}]' "
                + "highlighted-date='[{\"day\": \"2021-01-05\"}, {\"day\": \"2021-01-26\"}]' "
                + "format=\"ddMMyyyy\" "
                + "placeholder=\"placeholder01\" required=\"true\"/>",
            Stream.of(new Object[][] {
                {BiFields.PLACEHOLDER.getValue(), 1},
                {BiFields.TITLE.getValue(), 1},
                {BiFields.LABEL.getValue(), 1},
                {BiFields.DEFAULT.getValue(), 1},
                {BiFields.VALIDATION.getValue(), 1},
                {BiFields.VALIDATION_OPTIONS.getValue(), 1},
                {BiFields.VALIDATION_MIN.getValue(), 1},
                {BiFields.VALIDATION_MAX.getValue(), 1},
                {BiFields.VALIDATION_PATTERN.getValue(), 1},
                {BiFields.HIGHLIGHTED_OPTIONS.getValue(), 1},
                {BiFields.REQUIRED.getValue(), 1},
            }).collect(Collectors.toMap(property -> property[0], property -> property[1]))),

        Arguments.of(
            "<date-picker name=\"rules\" value=\"2020-08-28\" label=\"label01\" "
                + "title=\"title01\" min=\"2021-01-04\" max=\"2021-01-27\" "
                + "disabled-date='[{\"from\": \"2021-01-13\", \"to\": \"2021-01-15\"},"
                + " {\"from\": \"2021-01-19\", \"to\": \"2021-01-21\"}, {\"day\": \"2021-01-06\"}, "
                + "{\"daysOfWeek\": [0,6]}]' "
                + "highlighted-date='[{\"day\": \"2021-01-05\"}, {\"day\": \"2021-01-26\"}]' "
                + "format=\"ddMMyyyy\" "
                + "placeholder=\"placeholder01\" required=\"true\"/>",
            Stream.of(new Object[][] {
                {BiFields.PLACEHOLDER.getValue(), 1},
                {BiFields.TITLE.getValue(), 1},
                {BiFields.LABEL.getValue(), 1},
                {BiFields.DEFAULT.getValue(), 1},
                {BiFields.VALIDATION.getValue(), 1},
                {BiFields.VALIDATION_OPTIONS.getValue(), 1},
                {BiFields.VALIDATION_MIN.getValue(), 1},
                {BiFields.VALIDATION_MAX.getValue(), 1},
                {BiFields.VALIDATION_PATTERN.getValue(), 1},
                {BiFields.HIGHLIGHTED_OPTIONS.getValue(), 1},
                {BiFields.REQUIRED.getValue(), 1},
            }).collect(Collectors.toMap(property -> property[0], property -> property[1])))
    );
  }

  @ParameterizedTest
  @MethodSource("messageMlStream")
  void testBiContextDatePicker_withValidations(String dataPickerML, Map<String, Object> expectedAttributes)
      throws InvalidInputException, IOException, ProcessingException {
    MessageMLContext messageMLContext = new MessageMLContext(null);

    String input = String.format(
        "<messageML>\n "
            + "<form id=\"form_id\">\n "
            + "%s\n"
            + "<button name=\"name01\">Submit</button>"
            + "</form>\n </messageML>",
        dataPickerML);

    messageMLContext.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    List<BiItem> items = messageMLContext.getBiContext().getItems();

    BiItem datePickerBiItemExpected = new BiItem(BiFields.DATE_SELECTOR.getValue(), expectedAttributes);

    assertEquals(4, items.size());
    assertEquals(BiFields.DATE_SELECTOR.getValue(), items.get(0).getName());
    assertSameBiItem(datePickerBiItemExpected, items.get(0));
    assertMessageLengthBiItem(items.get(3), input.length());
  }

  @Test
  public void testDatePickerWithReadyOnlyAndDisabledAttributes() throws Exception {
    String input =
        "<messageML><form id=\"form_id\"><date-picker name=\"init\" value=\"2020-08-28\" "
            + "disabled=\"true\" readonly=\"true\"></date-picker><button name=\"submit\" "
            + "type=\"action\">Submit</button></form></messageML>";
    String expectedPresentationML =
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"form_id\"><input "
            + "type=\"date\" name=\"init\" value=\"2020-08-28\" disabled=\"true\" "
            + "readonly=\"true\"></input><button type=\"action\" "
            + "name=\"submit\">Submit</button></form></div>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals(expectedPresentationML, context.getPresentationML());
  }

  @Test
  public void testDatePickerWithInvalidReadyOnlyAttribute() throws Exception {
    String input =
        "<messageML><form id=\"form_id\"><date-picker name=\"init\" value=\"2020-08-28\" "
            + "readonly=\"invalid\"></date-picker><button name=\"submit\" "
            + "type=\"action\">Submit</button></form></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "Attribute \"readonly\" of element \"date-picker\" can only be one of the following "
            + "values: "
            + "[true, false].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDatePickerWithInvalidDisabledAttribute() throws Exception {
    String input =
        "<messageML><form id=\"form_id\"><date-picker name=\"init\" value=\"2020-08-28\" "
            + "disabled=\"invalid\"></date-picker><button name=\"submit\" "
            + "type=\"action\">Submit</button></form></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "Attribute \"disabled\" of element \"date-picker\" can only be one of the following "
            + "values: "
            + "[true, false].");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }




}
