package org.symphonyoss.symphony.messageml.elements;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.TimePickerNode;
import org.symphonyoss.symphony.messageml.util.XMLAttribute;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.symphonyoss.symphony.messageml.util.pojo.TimeInterval;
import org.w3c.dom.Node;

import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

public class TimePicker extends FormElement implements LabelableElement, TooltipableElement {

  public static final String MESSAGEML_TAG = "time-picker";
  public static final String PRESENTATIONML_INPUT_TYPE = "time";

  private static final String PRESENTATIONML_TAG = "input";
  private static final String VALUE_ATTR = "value";
  private static final String REQUIRED_ATTR = "required";
  private static final String PLACEHOLDER_ATTR = "placeholder";
  private static final String MIN_ATTR = "min";
  private static final String MAX_ATTR = "max";
  private static final String DISABLED_TIME_ATTR = "disabled-time";
  private static final String STEP_ATTR = "step";
  private static final String STRICT_ATTR = "strict";
  private static final String FORMAT_ATTR = "format";

  // PresentationML specific attributes
  private static final String FORMAT_PRESENTATION_ATTR = "data-format";
  private static final String DISABLED_TIME_PRESENTATION_ATTR = "data-disabled-time";
  private static final String STRICT_PRESENTATION_ATTR = "data-strict";

  private static final int DISABLED_TIME_RANGE_MAX_LENGTH = 1024;
  private static final int DEFAULT_MAX_LENGTH = 64;
  private static final int MIN_STEP_ALLOWED = 600;
  private static final int MAX_STEP_ALLOWED = 43200;

  private static final String FORMAT_ATTR_PATTERN = "^[hHmsa: ]+$";
  private static final String TIME_FORMAT_ALLOWED = "HH:mm:ss";


  public TimePicker(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
                                Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case TITLE:
      case LABEL:
      case VALUE_ATTR:
      case STRICT_ATTR:
      case REQUIRED_ATTR:
      case PLACEHOLDER_ATTR:
      case MIN_ATTR:
      case MAX_ATTR:
      case STEP_ATTR:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case DISABLED_TIME_ATTR:
      case FORMAT_ATTR:
        if (this.format != FormatEnum.MESSAGEML) {
          throwInvalidInputException(item);
        }
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case ID_ATTR:
      case DISABLED_TIME_PRESENTATION_ATTR:
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
    assertAttributeMaxLength(PLACEHOLDER_ATTR, DEFAULT_MAX_LENGTH);

    assertAttrDisabledTime();
    assertAttrStepRange();
    assertAttrFormat();

    assertTimeFormat(MIN_ATTR, DateTimeFormatter.ofPattern(TIME_FORMAT_ALLOWED));
    assertTimeFormat(MAX_ATTR, DateTimeFormatter.ofPattern(TIME_FORMAT_ALLOWED));
    assertTimeFormat(VALUE_ATTR, DateTimeFormatter.ofPattern(TIME_FORMAT_ALLOWED));

    if (getAttribute(REQUIRED_ATTR) != null) {
      assertAttributeValue(REQUIRED_ATTR, Arrays.asList("true", "false"));
    }

    if (getAttribute(STRICT_ATTR) != null) {
      assertAttributeValue(STRICT_ATTR, Arrays.asList("true", "false"));
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
                               MessageMLContext context) {
    Map<String, Object> presentationAttrs = buildTimePickerInputAttributes();
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
    return new TimePickerNode(getAttribute(LABEL), getAttribute(TITLE), getAttribute(PLACEHOLDER_ATTR));
  }

  @Override
  public void updateBiContext(BiContext context) {
    Map<String, Object> attributesMapBi = new HashMap<>();

    this.putOneIfPresent(attributesMapBi, BiFields.TITLE.getValue(), TITLE);
    this.putOneIfPresent(attributesMapBi, BiFields.PLACEHOLDER.getValue(), PLACEHOLDER_ATTR);
    this.putOneIfPresent(attributesMapBi, BiFields.LABEL.getValue(), LABEL);
    this.putOneIfPresent(attributesMapBi, BiFields.REQUIRED.getValue(), REQUIRED_ATTR);
    this.putOneIfPresent(attributesMapBi, BiFields.DEFAULT.getValue(), VALUE_ATTR);
    this.putIntegerIfPresent(attributesMapBi, BiFields.INPUT_STEP.getValue(), STEP_ATTR);
    this.computeAndPutValidationProperties(attributesMapBi);

    context.addItem(new BiItem(BiFields.TIME_PICKER.getValue(), attributesMapBi));
  }

  private void computeAndPutValidationProperties(Map<String, Object> attributesMapBi) {
    boolean validationMin = getAttribute(MIN_ATTR) != null;
    boolean validationMax = getAttribute(MAX_ATTR) != null;
    boolean validationPattern = getAttribute(FORMAT_ATTR) != null;
    boolean validationOptions = getAttribute(DISABLED_TIME_ATTR) != null;
    boolean validationStrict = getAttribute(STRICT_ATTR) != null;
    boolean hasValidation = validationMin || validationMax || validationPattern || validationOptions
        || validationStrict;

    if (validationMin) {
      attributesMapBi.put(BiFields.VALIDATION_MIN.getValue(), 1);
    }

    if (validationMax) {
      attributesMapBi.put(BiFields.VALIDATION_MAX.getValue(), 1);
    }

    if (validationPattern) {
      attributesMapBi.put(BiFields.VALIDATION_PATTERN.getValue(), 1);
    }

    if (validationOptions) {
      attributesMapBi.put(BiFields.VALIDATION_OPTIONS.getValue(), 1);
    }

    if (validationStrict) {
      attributesMapBi.put(BiFields.VALIDATION_STRICT.getValue(), 1);
    }

    if (hasValidation) {
      attributesMapBi.put(BiFields.VALIDATION.getValue(), 1);
    }
  }

  private void innerAsPresentationML(XmlPrintStream out, Map<String, Object> presentationAttrs) {
    out.printElement(PRESENTATIONML_TAG, presentationAttrs);
  }

  private void assertAttrDisabledTime() throws InvalidInputException {
    String disabledTime = getAttribute(DISABLED_TIME_ATTR);
    if (disabledTime == null) {
      return;
    }
    if (disabledTime.length() > DISABLED_TIME_RANGE_MAX_LENGTH) {
      throw new InvalidInputException(
              String.format("Attribute \"%s\" exceed maximum allowed length (%d)", DISABLED_TIME_ATTR,
                      DISABLED_TIME_RANGE_MAX_LENGTH));
    }
    try {
      TimeInterval[] timeIntervals = MAPPER.readValue(disabledTime, TimeInterval[].class);
      for (TimeInterval timeInterval : timeIntervals) {
        timeInterval.assertIsValid();
      }
    } catch (JsonProcessingException e) {
      throw new InvalidInputException(
              String.format("Error parsing json in attribute \"%s\": %s", DISABLED_TIME_ATTR,
                      e.getMessage()), e);
    }

  }

  private void assertAttrStepRange() throws InvalidInputException {
    String stepAttr = getAttribute(STEP_ATTR);
    if (stepAttr == null) {
      return;
    }
    int stepValue;
    try {
      stepValue = Integer.parseInt(stepAttr);
    } catch (NumberFormatException e) {
      throw new InvalidInputException(
              String.format("Attribute \"%s\" should be a number.", STEP_ATTR)
      );
    }
    if (stepValue < MIN_STEP_ALLOWED || stepValue > MAX_STEP_ALLOWED) {
      throw new InvalidInputException(
              String.format("Attribute \"%s\" must be less than \"%s\" and more than \"%s\".", STEP_ATTR, MAX_STEP_ALLOWED, MIN_STEP_ALLOWED));
    }
  }

  private void assertAttrFormat() throws InvalidInputException {
    assertAttributeMaxLength(FORMAT_ATTR, DEFAULT_MAX_LENGTH);
    String format = getAttribute(FORMAT_ATTR);
    if (format == null) {
      return;
    }
    if (!format.matches(FORMAT_ATTR_PATTERN)) {
      throw new InvalidInputException(
              String.format("Attribute \"%s\" contains an unsupported time format, only 'h', 'm' " +
                      "'s' and 'a' are supported with ':' or space as separator", FORMAT_ATTR));
    }
    try {
      DateTimeFormatter.ofPattern(format);
    } catch (IllegalArgumentException i) {
      throw new InvalidInputException(
              String.format("Attribute \"%s\" contains an invalid time format", FORMAT_ATTR));
    }
  }

  private Map<String, Object> buildTimePickerInputAttributes() {
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
    if (getAttribute(STEP_ATTR) != null) {
      presentationAttrs.put(STEP_ATTR, getAttribute(STEP_ATTR));
    }
    if (getAttribute(FORMAT_ATTR) != null) {
      presentationAttrs.put(FORMAT_PRESENTATION_ATTR, getAttribute(FORMAT_ATTR));
    }
    if (getAttribute(STRICT_ATTR) != null) {
      presentationAttrs.put(STRICT_PRESENTATION_ATTR, getAttribute(STRICT_ATTR));
    }
    if (getAttribute(DISABLED_TIME_ATTR) != null) {
      presentationAttrs.put(DISABLED_TIME_PRESENTATION_ATTR, convertJsonTimeToPresentationML(DISABLED_TIME_ATTR));
    }
    if (getAttribute(REQUIRED_ATTR) != null) {
      presentationAttrs.put(REQUIRED_ATTR, getAttribute(REQUIRED_ATTR));
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
  private XMLAttribute convertJsonTimeToPresentationML(String attributeName) {
    try {
      TimeInterval[] timeIntervals = MAPPER.readValue(getAttribute(attributeName), TimeInterval[].class);
      String result = MAPPER.writeValueAsString(timeIntervals);
      return XMLAttribute.of(result, XMLAttribute.Format.JSON);
    } catch (JsonProcessingException e) {
      // this exception should never happens because this method is called after validation
      throw new IllegalArgumentException(e);
    }
  }


}
