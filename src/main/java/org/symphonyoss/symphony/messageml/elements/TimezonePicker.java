package org.symphonyoss.symphony.messageml.elements;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.TimezonePickerNode;
import org.symphonyoss.symphony.messageml.util.XMLAttribute;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Node;

import java.time.DateTimeException;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * This class represents the Symphony Element Timezone Picker which is represented with tag name "timezone-picker".
 * The messageML representation of the element can contain the following fields:
 * <ul>
 *   <li> name (required) -> used to identify the picker </li>
 *   <li> label -> displayed on top of the Element </li>
 *   <li> title -> description displayed as a hint </li>
 *   <li> placeholder -> additional information </li>
 *   <li> value -> default timezone, could be enforced to "" </li>
 *   <li> required -> specifies that the input field must be filled out before submitting the form </li>
 *   <li> disabled-timezone -> json object containing list of timezones to be excluded </li>
 * </ul>
 *   Value attribute and timezones defined in the disabled-timezone should be valid and belonging to the
 *   "tz database"
 */

public class TimezonePicker extends FormElement implements LabelableElement, TooltipableElement {

  public static final String MESSAGEML_TAG = "timezone-picker";

  private static final String VALUE_ATTR = "value";
  private static final String REQUIRED_ATTR = "required";
  private static final String PLACEHOLDER_ATTR = "placeholder";
  private static final String DISABLED_TIMEZONE_ATTR = "disabled-timezone";

  public static final String PRESENTATIONML_CLASS = MESSAGEML_TAG;
  private static final String PRESENTATIONML_NAME_ATTR = "data-name";
  private static final String PRESENTATIONML_VALUE_ATTR = "data-value";
  private static final String PRESENTATIONML_REQUIRED_ATTR = "data-required";
  private static final String PRESENTATIONML_PLACEHOLDER_ATTR = "data-placeholder";
  private static final String PRESENTATIONML_DISABLED_TIMEZONE_ATTR = "data-disabled-timezone";

  private static final int DISABLED_TIMEZONE_MAX_LENGTH = 1024;
  private static final String PRESENTATIONML_TAG = "div";
  private static final String CLASS_ATTR = "class";

  public TimezonePicker(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
                                Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case PRESENTATIONML_NAME_ATTR:
        setAttribute(NAME_ATTR, getStringAttribute(item));
        break;
      case VALUE_ATTR:
      case PRESENTATIONML_VALUE_ATTR:
        setAttribute(VALUE_ATTR, getStringAttribute(item));
        break;
      case REQUIRED_ATTR:
      case PRESENTATIONML_REQUIRED_ATTR:
        setAttribute(REQUIRED_ATTR, getStringAttribute(item));
        break;
      case PLACEHOLDER_ATTR:
      case PRESENTATIONML_PLACEHOLDER_ATTR:
        setAttribute(PLACEHOLDER_ATTR, getStringAttribute(item));
        break;
      case DISABLED_TIMEZONE_ATTR:
      case PRESENTATIONML_DISABLED_TIMEZONE_ATTR:
        setAttribute(DISABLED_TIMEZONE_ATTR, getStringAttribute(item));
        break;
      case ID_ATTR:
      case TITLE:
      case LABEL:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();
    assertAttributeNotBlank(NAME_ATTR);

    if (getAttribute(REQUIRED_ATTR) != null) {
      assertAttributeValue(REQUIRED_ATTR, Arrays.asList("true", "false"));
    }

    if (getAttribute(VALUE_ATTR) != null && !getAttribute(VALUE_ATTR).isEmpty()) {
      assertTimezoneValid(VALUE_ATTR, getAttribute(VALUE_ATTR));
    }

    assertAttributeMaxLength(DISABLED_TIMEZONE_ATTR, DISABLED_TIMEZONE_MAX_LENGTH);
    assertAttrDisabledTimezone(DISABLED_TIMEZONE_ATTR);
  }

  private void assertAttrDisabledTimezone(String attributeName) throws InvalidInputException {
    String disabledTimezones = getAttribute(attributeName);
    if (disabledTimezones == null) return;
    try {
      List<String> timezones = MAPPER.readValue(disabledTimezones,
              MAPPER.getTypeFactory().constructCollectionType(List.class, String.class));
      for (String timezone : timezones) {
        assertTimezoneValid(attributeName, timezone);
      }
    } catch (JsonProcessingException e) {
      throw new InvalidInputException(
              String.format("Error parsing json in attribute \"%s\". It should contain an array of Strings",
                      attributeName), e);
    }
  }

  private void assertTimezoneValid(String attributeValue, String timezone)
          throws InvalidInputException {
    try {
      ZoneId.of(timezone);
    } catch (DateTimeException e) {
      throw new InvalidInputException(String.format("Attribute \"%s\" contains an invalid timezone: %s", attributeValue, timezone), e);
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
                               MessageMLContext context) {
    Map<String, Object> presentationAttrs = buildTimezonePickerInputAttributes();
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
  public void updateBiContext(BiContext context) {
    Map<String, Object> attributesMapBi = new HashMap<>();

    this.putOneIfPresent(attributesMapBi, BiFields.TITLE.getValue(), TITLE);
    this.putOneIfPresent(attributesMapBi, BiFields.LABEL.getValue(), LABEL);
    this.putOneIfPresent(attributesMapBi, BiFields.PLACEHOLDER.getValue(), PLACEHOLDER_ATTR);
    this.putOneIfPresent(attributesMapBi, BiFields.REQUIRED.getValue(), REQUIRED_ATTR);
    this.putOneIfPresent(attributesMapBi, BiFields.DEFAULT.getValue(), VALUE_ATTR);
    this.computeAndPutValidationProperties(attributesMapBi);

    context.addItem(new BiItem(BiFields.TIMEZONE_PICKER.getValue(), attributesMapBi));
  }

  private void computeAndPutValidationProperties(Map<String, Object> attributesMapBi) {
    boolean validationOptions = getAttribute(DISABLED_TIMEZONE_ATTR) != null;

    if (validationOptions) {
      attributesMapBi.put(BiFields.VALIDATION_OPTIONS.getValue(), 1);
      attributesMapBi.put(BiFields.VALIDATION.getValue(), 1);
    }
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new TimezonePickerNode(getAttribute(LABEL), getAttribute(TITLE), getAttribute(PLACEHOLDER_ATTR));
  }

  private void innerAsPresentationML(XmlPrintStream out, Map<String, Object> presentationAttrs) {
    out.openElement(PRESENTATIONML_TAG, presentationAttrs);
    out.closeElement();
  }

  private Map<String, Object> buildTimezonePickerInputAttributes() {
    Map<String, Object> presentationAttrs = new LinkedHashMap<>();

    presentationAttrs.put(CLASS_ATTR, MESSAGEML_TAG);
    presentationAttrs.put(PRESENTATIONML_NAME_ATTR, getAttribute(NAME_ATTR));

    if (getAttribute(VALUE_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_VALUE_ATTR, getAttribute(VALUE_ATTR));
    }
    if (getAttribute(PLACEHOLDER_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_PLACEHOLDER_ATTR, getAttribute(PLACEHOLDER_ATTR));
    }

    if (getAttribute(DISABLED_TIMEZONE_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_DISABLED_TIMEZONE_ATTR, convertJsonTimezoneToPresentationML(DISABLED_TIMEZONE_ATTR));
    }

    if (getAttribute(REQUIRED_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_REQUIRED_ATTR, getAttribute(REQUIRED_ATTR));
    }
    return presentationAttrs;
  }

  private XMLAttribute convertJsonTimezoneToPresentationML(String attributeName) {
    try {
      List<String> timezones = MAPPER.readValue(getAttribute(attributeName),
              MAPPER.getTypeFactory().constructCollectionType(List.class, String.class));
      String result = MAPPER.writeValueAsString(timezones);
      return XMLAttribute.of(result, XMLAttribute.Format.JSON);
    } catch (JsonProcessingException e) {
      // this exception should never happens because this method is called after validation
      throw new IllegalArgumentException(e);
    }
  }
}
