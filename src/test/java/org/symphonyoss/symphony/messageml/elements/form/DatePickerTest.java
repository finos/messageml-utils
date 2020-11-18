package org.symphonyoss.symphony.messageml.elements.form;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.MessageML;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
  public void testDatePicker() throws Exception {
    String input = "<messageML><form id=\"" + formId + "\">"
        + "<date-picker \n"
        + "      name=\"date-travel\"\n"
        + "      value=\"2020-09-15\"\n"
        + "      title=\"This is \\n a hint\"\n"
        + "      label=\"Departure date\"\n"
        + "      placeholder=\"Please pick a date\"\n"
        + "      min=\"2020-09-01\"\n"
        + "      max=\"2020-09-29\"\n"
        + "      disabled-date=\"[{'day': '2020-09-23'}, {'from': '2020-09-18', 'to': '2020-09-20'}, "
        + "{'daysOfWeek': [0,1]}]\""
        + "      highlighted-date=\"[{'day': '2020-09-24'}, {'from': '2020-09-26', 'to': '2020-09-28'}, "
        + "{'day': '2020-09-03'}, {'daysOfWeek': [4,6]}]\""
        + "      required=\"true\"\n"
        + "      format=\"yyyy-MM-dd\"\n"
        + "    />"
        + ACTION_BTN_ELEMENT + "</form></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    String presentationML = context.getPresentationML();
    String datePickerRegex = ".*(\"date-picker-(.*?)\").*";
    Pattern pattern = Pattern.compile(datePickerRegex);
    Matcher matcher = pattern.matcher(presentationML);
    String uniqueLabelId = matcher.matches() ? matcher.group(2) : null;

    String expectedPresentationML = String.format("<div data-format=\"PresentationML\" "
        + "data-version=\"2.0\"><form id=\"datepicker-form\"><div class=\"date-picker-group\" "
        + "data-generated=\"true\"><label for=\"date-picker-%s\">Departure "
        + "date</label><span class=\"info-hint\" data-target-id=\"date-picker-%s\" "
        + "data-title=\"This is \\n a hint\"></span><input name=\"date-travel\" "
        + "value=\"2020-09-15\" placeholder=\"Please pick a date\" min=\"2020-09-01\" "
        + "max=\"2020-09-29\" data-disabled-date=\"[{'type': 'date', 'day': '2020-09-23'}, "
        + "{'type': 'range', 'from': '2020-09-18', 'to': '2020-09-20'}, {'type': 'weekdays', "
        + "'daysOfWeek': [ 0, 1 ]}]\" data-highlighted-date=\"[{'type': 'date', 'day': "
        + "'2020-09-24'}, {'type': 'range', 'from': '2020-09-26', 'to': '2020-09-28'}, {'type': "
        + "'date', 'day': '2020-09-03'}, {'type': 'weekdays', 'daysOfWeek': [ 4, 6 ]}]\" "
        + "required=\"true\" data-format=\"yyyy-MM-dd\" "
        + "id=\"date-picker-%s\"></input></div><button type=\"action\" "
        + "name=\"actionName\">Send</button></form></div>", uniqueLabelId, uniqueLabelId, uniqueLabelId, uniqueLabelId);
    Assert.assertEquals(expectedPresentationML, presentationML);
  }

}
