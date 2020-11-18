package org.symphonyoss.symphony.messageml.elements;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.Range;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Node;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author enrico.molino (16/11/2020)
 */
public class DatePicker extends FormElement implements LabelableElement, TooltipableElement {

  public static final String MESSAGEML_TAG = "date-picker";
  public static final String PRESENTATIONML_INPUT_TYPE = "date";
  private static final String PRESENTATIONML_TAG = "input";
  private static final String VALUE_ATTR = "value";
  private static final String REQUIRED_ATTR = "required";
  private static final String PLACEHOLDER_ATTR = "placeholder";
  private static final String MIN_ATTR = "min";
  private static final String MAX_ATTR = "max";
  private static final String DISABLED_DATE_ATTR = "disabled-date";
  private static final String HIGHLIGHTED_DATE_ATTR = "highlighted-date";
  private static final String FORMAT_ATTR = "format";

  private static final int DATE_RANGE_MAX_LENGTH = 1024;
  private static final String TYPE = "type";
  private static final String TYPE_DATE = "date";
  private static final String TYPE_RANGE = "range";
  private static final String TYPE_WEEKDAYS = "weekdays";
  private static final String FROM = "from";
  private static final String TO = "to";
  private static final String DAY = "day";
  private static final String DAYS_OF_WEEK = "daysOfWeek";

  // PresentationML specific attributes
  private static final String DISABLED_DATE_PRESENTATION_ATTR = "data-disabled-date";
  private static final String HIGHLIGHTED_DATE_PRESENTATION_ATTR = "data-highlighted-date";
  private static final String FORMAT_PRESENTATION_ATTR = "data-format";

  private static final Range<Integer> ALLOWED_DAYS = Range.between(0, 6);

  private final ObjectMapper mapper;

  public DatePicker(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
    mapper = new ObjectMapper();
    mapper.configure(JsonParser.Feature.ALLOW_SINGLE_QUOTES, true);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case VALUE_ATTR:
      case REQUIRED_ATTR:
      case PLACEHOLDER_ATTR:
      case MIN_ATTR:
      case MAX_ATTR:
      case LABEL:
      case TITLE:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case DISABLED_DATE_ATTR:
      case HIGHLIGHTED_DATE_ATTR:
      case FORMAT_ATTR:
        if(this.format != FormatEnum.MESSAGEML){
          throwInvalidInputException(item);
        }
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case ID_ATTR:
      case DISABLED_DATE_PRESENTATION_ATTR:
      case HIGHLIGHTED_DATE_PRESENTATION_ATTR:
      case FORMAT_PRESENTATION_ATTR:
        if(this.format != FormatEnum.PRESENTATIONML){
          throwInvalidInputException(item);
        }
        fillAttributes(parser, item);
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    assertAttributeNotBlank(NAME_ATTR);

    if(getAttribute(VALUE_ATTR) != null) {
      assertDateFormat(VALUE_ATTR, DateTimeFormatter.ISO_DATE);
    }
    if(getAttribute(REQUIRED_ATTR) != null) {
      assertAttributeValue(REQUIRED_ATTR, Arrays.asList("true", "false"));
    }
    if(getAttribute(MIN_ATTR) != null){
      assertDateFormat(MIN_ATTR, DateTimeFormatter.ISO_DATE);
    }
    if(getAttribute(MAX_ATTR) != null){
      assertDateFormat(MAX_ATTR, DateTimeFormatter.ISO_DATE);
    }
    assertJsonDatesRange(DISABLED_DATE_ATTR);
    assertJsonDatesRange(HIGHLIGHTED_DATE_ATTR);

    if(getAttribute(FORMAT_ATTR) != null){
      try {
        DateTimeFormatter.ofPattern(getAttribute(FORMAT_ATTR));
      } catch (IllegalArgumentException i){
        throw new InvalidInputException(String.format("Attribute \"%s\" contains an invalid date format", FORMAT_ATTR));
      }
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    Map<String, String> presentationAttrs = buildDataPickerInputAttributes();
    if(isSplittable()){
      // open div + adding splittable elements
      presentationAttrs.put(ID_ATTR, splittableAsPresentationML(out, context));
      // render element
      innerAsPresentationML(out, presentationAttrs);
      // close div
      out.closeElement();
    } else {
      innerAsPresentationML(out, presentationAttrs);
    }
  }

  private void innerAsPresentationML(XmlPrintStream out, Map<String, String> presentationAttrs){
    out.openElement(PRESENTATIONML_TAG, presentationAttrs);
    out.closeElement();
  }

  private Map<String, String> buildDataPickerInputAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();

    presentationAttrs.put(NAME_ATTR, getAttribute(NAME_ATTR));

    if(getAttribute(VALUE_ATTR) != null) {
      presentationAttrs.put(VALUE_ATTR, getAttribute(VALUE_ATTR));
    }
    if(getAttribute(PLACEHOLDER_ATTR) != null) {
      presentationAttrs.put(PLACEHOLDER_ATTR, getAttribute(PLACEHOLDER_ATTR));
    }
    if(getAttribute(MIN_ATTR) != null) {
      presentationAttrs.put(MIN_ATTR, getAttribute(MIN_ATTR));
    }
    if(getAttribute(MAX_ATTR) != null) {
      presentationAttrs.put(MAX_ATTR, getAttribute(MAX_ATTR));
    }
    if(getAttribute(DISABLED_DATE_ATTR) != null) {
      presentationAttrs.put(DISABLED_DATE_PRESENTATION_ATTR, convertJsonDateToPresentationML(DISABLED_DATE_ATTR));
    }
    if(getAttribute(HIGHLIGHTED_DATE_ATTR) != null) {
      presentationAttrs.put(HIGHLIGHTED_DATE_PRESENTATION_ATTR, convertJsonDateToPresentationML(HIGHLIGHTED_DATE_ATTR));
    }
    if(getAttribute(REQUIRED_ATTR) != null) {
      presentationAttrs.put(REQUIRED_ATTR, getAttribute(REQUIRED_ATTR));
    }
    if(getAttribute(FORMAT_ATTR) != null) {
      presentationAttrs.put(FORMAT_PRESENTATION_ATTR, getAttribute(FORMAT_ATTR));
    }
    return presentationAttrs;
  }

  /**
   * The Json for PresentationML is different from MessageML
   * It needs to be rewritten, by adding the type attribute, based on the content
   *
   * @param attributeName
   * @return
   */
  private String convertJsonDateToPresentationML(String attributeName) {
    StringBuilder result = new StringBuilder("[");
    boolean isFirst = true;
    try {
      for (JsonNode jsonNode : mapper.readTree(getAttribute(attributeName))) {
        if(isFirst){
          isFirst = false;
        } else {
          result.append(", ");
        }
        result.append("{").append("'").append(TYPE).append("': '");
        if(jsonNode.get(FROM) != null){
          result.append(TYPE_RANGE).append("', '")
              .append(FROM).append("': '").append(jsonNode.get(FROM).asText()).append("', '")
              .append(TO).append("': '").append(jsonNode.get(TO).asText()).append("'");
        } else if(jsonNode.get(DAY) != null){
          result.append(TYPE_DATE).append("', '")
              .append(DAY).append("': '").append(jsonNode.get(DAY).asText()).append("'");
        } else if(jsonNode.get(DAYS_OF_WEEK) != null){
          result.append(TYPE_WEEKDAYS).append("', '")
              .append(DAYS_OF_WEEK).append("': ").append(jsonNode.get(DAYS_OF_WEEK).toPrettyString());
        } else {
          // this exception should never happens because this method is called after validation
          throw new InvalidInputException(String.format("Attribute \"%s\" has invalid date range in json data", attributeName));
        }
        result.append("}");
      }
    } catch (JsonProcessingException | InvalidInputException e) {
      // this exception should never happens because this method is called after validation
      throw new IllegalArgumentException(e);
    }
    result.append("]");
    return result.toString();
  }

  /**
   * Validates date ranges attributes, they should contains a json of a similar format:
   *
   *[
   *  {from: ‘YYYY-MM-DD’, to: ‘YYYY-MM-DD’},
   *  {day: ‘YYYY-MM-DD’ },
   *  {day: ‘YYYY-MM-DD’ },
   *  {day: ‘YYYY-MM-DD’ },
   *  {from: ‘YYYY-MM-DD’, to: ‘YYYY-MM-DD’},
   *  {daysOfWeek: [0, 1]}
   * ]
   *
   */
  private void assertJsonDatesRange(String attributeName) throws InvalidInputException {
    String attributeValue = getAttribute(attributeName);
    if (attributeValue != null) {
      if (attributeValue.length() > DATE_RANGE_MAX_LENGTH) {
        throw new InvalidInputException(
            String.format("Attribute \"%s\" exceed maximum allowed length (%d)", attributeName,
                DATE_RANGE_MAX_LENGTH));
      }
      try {
        JsonNode arrayNode = mapper.readTree(attributeValue);
        if (!arrayNode.isArray()) {
          throw new InvalidInputException(
              String.format("Attribute \"%s\" json must be an array", attributeName));
        }
        for (JsonNode jsonNode : arrayNode) {
          Iterator<Map.Entry<String, JsonNode>> iterator = jsonNode.fields();
          int from = 0;
          int to = 0;
          int day = 0;
          int daysOfWeek = 0;
          while (iterator.hasNext()) {
            Map.Entry<String, JsonNode> field = iterator.next();
            switch (field.getKey()) {
              case FROM:
                assertDateFormat(field.getValue(), attributeName);
                from++;
                break;
              case TO:
                assertDateFormat(field.getValue(), attributeName);
                to++;
                break;
              case DAY:
                assertDateFormat(field.getValue(), attributeName);
                day++;
                break;
              case DAYS_OF_WEEK:
                assertDaysOfWeek(field.getValue(), attributeName);
                daysOfWeek++;
                break;
              default:
                throw new InvalidInputException(
                    String.format("Attribute \"%s\" has invalid field %s in json data", attributeName, field.getKey()));
            }
          }
          // Verify that if there is a from, also a to is present
          if((from > 0 || to > 0) && (from != 1 || to != 1)){
            throw new InvalidInputException(
                String.format("Attribute \"%s\" has invalid %s - %s date range in json data", attributeName, FROM, TO));
          }
          // verify that there is only a type of range, from/to OR day OR daysWeek
          int sum = from + to + day + daysOfWeek;
          if((from == 0 && sum != 1) || (from == 1 && sum != 2)){
            throw new InvalidInputException(
                String.format("Attribute \"%s\" has invalid date range in json data", attributeName));
          }
        }
      } catch (JsonProcessingException e) {
        throw new InvalidInputException(
            String.format("Attribute \"%s\" invalid json content", attributeName), e);
      }
    }
  }

  private void assertDateFormat(JsonNode date, String attributeName) throws InvalidInputException {
    try {
      LocalDate.parse(date.asText(), DateTimeFormatter.ISO_DATE);
    } catch (DateTimeParseException e) {
      throw new InvalidInputException(String.format("Attribute \"%s\" has invalid date format in json data", attributeName), e);
    }
  }

  private void assertDaysOfWeek(JsonNode daysOfWeek, String attributeName) throws InvalidInputException {
    if(daysOfWeek.isArray()){
      for (JsonNode day : daysOfWeek) {
        if(!day.isNumber() || !ALLOWED_DAYS.contains(day.asInt())){
          throw new InvalidInputException(String.format("Attribute \"%s\" has invalid %s in json data, 0-6 only is allowed", attributeName, DAYS_OF_WEEK));
        }
      }
      return;
    }
    throw new InvalidInputException(String.format("Attribute \"%s\" has invalid %s in json data, an array is expected", attributeName, DAYS_OF_WEEK));
  }

}
