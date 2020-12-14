package org.symphonyoss.symphony.messageml.elements;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.DatePickerNode;
import org.symphonyoss.symphony.messageml.util.XMLAttribute;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.symphonyoss.symphony.messageml.util.pojo.DateInterval;
import org.w3c.dom.Node;

import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Class representing a DatePicker inside a Form.
 *
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
  private static final int DEFAULT_MAX_LENGTH = 64;

  // PresentationML specific attributes
  private static final String DISABLED_DATE_PRESENTATION_ATTR = "data-disabled-date";
  private static final String HIGHLIGHTED_DATE_PRESENTATION_ATTR = "data-highlighted-date";
  private static final String FORMAT_PRESENTATION_ATTR = "data-format";

  private final ObjectMapper mapper;

  private static final String dateFormatAllowed = "^[0-9Mdy\\/. -]+$";

  public DatePicker(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
    mapper = new ObjectMapper();
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
        if (this.format != FormatEnum.MESSAGEML) {
          throwInvalidInputException(item);
        }
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case ID_ATTR:
      case DISABLED_DATE_PRESENTATION_ATTR:
      case HIGHLIGHTED_DATE_PRESENTATION_ATTR:
      case FORMAT_PRESENTATION_ATTR:
        if (this.format != FormatEnum.PRESENTATIONML) {
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
    assertAttributeMaxLength(NAME_ATTR, DEFAULT_MAX_LENGTH);

    if (getAttribute(VALUE_ATTR) != null) {
      assertDateFormat(VALUE_ATTR, DateTimeFormatter.ISO_DATE);
    }
    if (getAttribute(REQUIRED_ATTR) != null) {
      assertAttributeValue(REQUIRED_ATTR, Arrays.asList("true", "false"));
    }
    if (getAttribute(MIN_ATTR) != null) {
      assertDateFormat(MIN_ATTR, DateTimeFormatter.ISO_DATE);
    }
    if (getAttribute(MAX_ATTR) != null) {
      assertDateFormat(MAX_ATTR, DateTimeFormatter.ISO_DATE);
    }
    assertJsonDatesRange(DISABLED_DATE_ATTR);
    assertJsonDatesRange(HIGHLIGHTED_DATE_ATTR);

    if (getAttribute(FORMAT_ATTR) != null) {
      assertAttributeMaxLength(FORMAT_ATTR, DEFAULT_MAX_LENGTH);
      String format = getAttribute(FORMAT_ATTR);
      if(!format.matches(dateFormatAllowed)){
        throw new InvalidInputException(
            String.format("Attribute \"%s\" contains an unsupported date format, only 'M', 'd' and 'y' are supported with a space or '.','-','/' as separator", FORMAT_ATTR)
        );
      }
      try {
        DateTimeFormatter.ofPattern(getAttribute(FORMAT_ATTR)).toFormat();
      } catch (IllegalArgumentException i) {
        throw new InvalidInputException(
            String.format("Attribute \"%s\" contains an invalid date format", FORMAT_ATTR));
      }
    }
    assertAttributeMaxLength(TITLE, DEFAULT_MAX_LENGTH);
    assertAttributeMaxLength(LABEL, DEFAULT_MAX_LENGTH);
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    Map<String, Object> presentationAttrs = buildDataPickerInputAttributes();
    if (isSplittable()) {
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

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new DatePickerNode(getAttribute(LABEL), getAttribute(TITLE), getAttribute(PLACEHOLDER_ATTR));
  }

  private void innerAsPresentationML(XmlPrintStream out, Map<String, Object> presentationAttrs) {
    out.openElement(PRESENTATIONML_TAG, presentationAttrs);
    out.closeElement();
  }

  private Map<String, Object> buildDataPickerInputAttributes() {
    Map<String, Object> presentationAttrs = new LinkedHashMap<>();

    presentationAttrs.put(TYPE_ATTR, PRESENTATIONML_INPUT_TYPE);
    presentationAttrs.put(NAME_ATTR, getAttribute(NAME_ATTR));

    if (getAttribute(VALUE_ATTR) != null) {
      presentationAttrs.put(VALUE_ATTR, getAttribute(VALUE_ATTR));
    }
    if (getAttribute(PLACEHOLDER_ATTR) != null) {
      presentationAttrs.put(PLACEHOLDER_ATTR, getAttribute(PLACEHOLDER_ATTR));
    }
    if (getAttribute(MIN_ATTR) != null) {
      presentationAttrs.put(MIN_ATTR, getAttribute(MIN_ATTR));
    }
    if (getAttribute(MAX_ATTR) != null) {
      presentationAttrs.put(MAX_ATTR, getAttribute(MAX_ATTR));
    }
    if (getAttribute(DISABLED_DATE_ATTR) != null) {
      presentationAttrs.put(DISABLED_DATE_PRESENTATION_ATTR,
          convertJsonDateToPresentationML(DISABLED_DATE_ATTR));
    }
    if (getAttribute(HIGHLIGHTED_DATE_ATTR) != null) {
      presentationAttrs.put(HIGHLIGHTED_DATE_PRESENTATION_ATTR,
          convertJsonDateToPresentationML(HIGHLIGHTED_DATE_ATTR));
    }
    if (getAttribute(REQUIRED_ATTR) != null) {
      presentationAttrs.put(REQUIRED_ATTR, getAttribute(REQUIRED_ATTR));
    }
    if (getAttribute(FORMAT_ATTR) != null) {
      presentationAttrs.put(FORMAT_PRESENTATION_ATTR, getAttribute(FORMAT_ATTR));
    }
    return presentationAttrs;
  }

  /**
   * The Json for PresentationML is different from MessageML It needs to be rewritten, by adding the
   * type attribute, based on the content
   *
   * @param attributeName
   * @return
   */
  private XMLAttribute convertJsonDateToPresentationML(String attributeName) {
    try {
      DateInterval[] dateIntervals = mapper.readValue(getAttribute(attributeName), DateInterval[].class);
      String result = mapper.writeValueAsString(dateIntervals);
      return XMLAttribute.of(result, XMLAttribute.Format.JSON);
    } catch (JsonProcessingException e) {
      // this exception should never happens because this method is called after validation
      throw new IllegalArgumentException(e);
    }
  }

  /**
   * Validates date ranges attributes, they should contains a json of a similar format:
   * <p>
   * [ {from: ‘YYYY-MM-DD’, to: ‘YYYY-MM-DD’}, {day: ‘YYYY-MM-DD’ }, {day: ‘YYYY-MM-DD’ }, {day:
   * ‘YYYY-MM-DD’ }, {from: ‘YYYY-MM-DD’, to: ‘YYYY-MM-DD’}, {daysOfWeek: [0, 1]} ]
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
        DateInterval[] dateIntervals = mapper.readValue(attributeValue, DateInterval[].class);
        for(DateInterval dateInterval:dateIntervals){
          dateInterval.assertIsValid();
        }
      } catch (JsonProcessingException e) {
        throw new InvalidInputException(
            String.format("Error parsing json in attribute \"%s\": %s", attributeName,
                e.getMessage()), e);
      }
    }
  }
}
