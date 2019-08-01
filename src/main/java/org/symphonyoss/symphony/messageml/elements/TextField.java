package org.symphonyoss.symphony.messageml.elements;

import static java.lang.String.format;

import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormElementNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;


/**
 * Class representing a Text Field inside a Form.
 * @author Lucas Macedo
 * @since 06/07/2019
 */
public class TextField extends FormElement {

  public static final String MESSAGEML_TAG = "text-field";
  public static final String PRESENTATIONML_INPUT_TYPE = "text";

  private static final String MINLENGTH_ATTR = "minlength";
  private static final String MAXLENGTH_ATTR = "maxlength";
  private static final String REQUIRED_ATTR = "required";
  private static final String MASKED_ATTR = "masked";
  private static final String PLACEHOLDER_ATTR = "placeholder";
  private static final String VALUE_ATTR = "value";

  private static final String PRESENTATIONML_MASKED_ATTR = "data-masked";

  private static final Set<String> VALID_BOOLEAN_VALUES = new HashSet<>(Arrays.asList("true", "false"));
  private static final Integer MIN_ALLOWED_LENGTH = 1;
  private static final Integer MAX_ALLOWED_LENGTH = 128;

  private final static String MARKDOWN = "Text Field";

  public TextField(Element parent, FormatEnum messageFormat) {
    super(parent, MESSAGEML_TAG, messageFormat);
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    if (getAttribute(NAME_ATTR) == null) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }

    if (getAttribute(REQUIRED_ATTR) != null) {
      assertAttributeValue(REQUIRED_ATTR, VALID_BOOLEAN_VALUES);
    }

    if (getAttribute(MASKED_ATTR) != null) {
      assertAttributeValue(MASKED_ATTR, VALID_BOOLEAN_VALUES);
    }

    assertAttributeNotBlank(NAME_ATTR);
    assertContentModel(Collections.singleton(TextNode.class));
    validateMinAndMaxLengths();
  }

  @Override
  public void buildAll(MessageMLParser context, org.w3c.dom.Element element)
      throws InvalidInputException, ProcessingException {
    switch (getFormat()) {
      case MESSAGEML:
        super.buildAll(context, element);
        break;
      case PRESENTATIONML:
        this.buildAllFromPresentationML(element);
        break;
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    out.printElement(INPUT_TAG, buildTextFieldInputAttributes());
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    if (getAttribute(PLACEHOLDER_ATTR) != null) {
      return new FormElementNode(MARKDOWN, ":" + getAttribute(PLACEHOLDER_ATTR));
    } else {
      return new FormElementNode(MARKDOWN);
    }
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
        setAttribute(NAME_ATTR, getStringAttribute(item));
        break;
      case REQUIRED_ATTR:
        setAttribute(REQUIRED_ATTR, getStringAttribute(item));
        break;
      case PLACEHOLDER_ATTR:
        setAttribute(PLACEHOLDER_ATTR, getStringAttribute(item));
        break;
      case MINLENGTH_ATTR:
        setAttribute(MINLENGTH_ATTR, getStringAttribute(item));
        break;
      case MAXLENGTH_ATTR:
        setAttribute(MAXLENGTH_ATTR, getStringAttribute(item));
        break;
      case MASKED_ATTR:
        setAttribute(MASKED_ATTR, getStringAttribute(item));
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  private void buildAllFromPresentationML(org.w3c.dom.Element element)
      throws InvalidInputException {
    NamedNodeMap attr = element.getAttributes();
    NodeList children = element.getChildNodes();

    if (children != null && children.getLength() > 0) {
      throw new InvalidInputException(
          "Element \"" + this.getMessageMLTag() + "\" may not have child elements or text content");
    }

    for (int i = 0; i < attr.getLength(); i++) {
      buildAttributeFromPresentationML(attr.item(i));
    }
  }

  private void buildAttributeFromPresentationML(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
        setAttribute(NAME_ATTR, getStringAttribute(item));
        break;
      case VALUE_ATTR:
        addChild(new TextNode(this, getStringAttribute(item)));
        break;
      case REQUIRED_ATTR:
        setAttribute(REQUIRED_ATTR, getStringAttribute(item));
        break;
      case PLACEHOLDER_ATTR:
        setAttribute(PLACEHOLDER_ATTR, getStringAttribute(item));
        break;
      case MINLENGTH_ATTR:
        setAttribute(MINLENGTH_ATTR, getStringAttribute(item));
        break;
      case MAXLENGTH_ATTR:
        setAttribute(MAXLENGTH_ATTR, getStringAttribute(item));
        break;
      case PRESENTATIONML_MASKED_ATTR:
        setAttribute(MASKED_ATTR, getStringAttribute(item));
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  private Map<String, String> buildTextFieldInputAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();

    presentationAttrs.put(TYPE_ATTR, PRESENTATIONML_INPUT_TYPE);
    presentationAttrs.put(NAME_ATTR, getAttribute(NAME_ATTR));

    if (getAttribute(PLACEHOLDER_ATTR) != null) {
      presentationAttrs.put(PLACEHOLDER_ATTR, getAttribute(PLACEHOLDER_ATTR));
    }

    if (getAttribute(REQUIRED_ATTR) != null) {
      presentationAttrs.put(REQUIRED_ATTR, getAttribute(REQUIRED_ATTR));
    }

    if (getAttribute(MASKED_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_MASKED_ATTR, getAttribute(MASKED_ATTR));
    }

    if (getAttribute(MINLENGTH_ATTR) != null) {
      presentationAttrs.put(MINLENGTH_ATTR, getAttribute(MINLENGTH_ATTR));
    }

    if (getAttribute(MAXLENGTH_ATTR) != null) {
      presentationAttrs.put(MAXLENGTH_ATTR, getAttribute(MAXLENGTH_ATTR));
    }

    if (getChildren() != null && getChildren().size() == 1) {
      presentationAttrs.put(VALUE_ATTR, getChildren().get(0).asText());
    }

    return presentationAttrs;
  }

  private void validateMinAndMaxLengths() throws InvalidInputException {
    Integer maxLength = getAttributeAsInteger(MAXLENGTH_ATTR);
    if (isLengthOutOfRange(maxLength)) {
      throw new InvalidInputException(getLengthErrorMessage(MAXLENGTH_ATTR));
    }

    Integer minLength = getAttributeAsInteger(MINLENGTH_ATTR);
    if (isLengthOutOfRange(minLength)) {
      throw new InvalidInputException(getLengthErrorMessage(MINLENGTH_ATTR));
    }

    minLength = getDefaultValueIfCurrentIsNull(minLength, MIN_ALLOWED_LENGTH);
    maxLength = getDefaultValueIfCurrentIsNull(maxLength, MAX_ALLOWED_LENGTH);

    if (isMinAndMaxLengthCombinationValid(maxLength, minLength)) {
      throw new InvalidInputException("The attribute \"minlength\" must be lower than the \"maxlength\" attribute");
    }

    if (textFieldHasInitialValue()) {
      String initialValue = getTextFieldInitialValue();
      if (isTextSmallerThanMinLength(minLength, initialValue) || isTextBiggerThanMaxLength(maxLength, initialValue)) {
        throw new InvalidInputException(String.format(
            "The length of this text-field's initial value must be between %s and %s", minLength, maxLength));
      }
    }
  }

  private Integer getDefaultValueIfCurrentIsNull(Integer currentValue, Integer defaultValue) {
    return currentValue == null ? defaultValue : currentValue;
  }

  private String getTextFieldInitialValue() {
    return ((TextNode) getChild(0)).getText();
  }

  private boolean isMinAndMaxLengthCombinationValid(Integer maxLength, Integer minLength) {
    return minLength != null && maxLength != null && minLength > maxLength;
  }

  private boolean isTextBiggerThanMaxLength(Integer maxLength, String text) {
    return text != null && maxLength != null && text.length() > maxLength;
  }

  private boolean isTextSmallerThanMinLength(Integer minLength, String text) {
    return text != null && minLength != null && text.length() < minLength;
  }

  private boolean textFieldHasInitialValue() {
    return getChildren() != null && getChildren().size() == 1 && getChild(0) instanceof TextNode;
  }

  private Integer getAttributeAsInteger(String attributeName) throws InvalidInputException {
    Integer length = null;

    if (getAttribute(attributeName) != null) {
      try {
        length = Integer.parseInt(getAttribute(attributeName));
      } catch (NumberFormatException e) {
        throw new InvalidInputException(format("The attribute \"%s\" must be a valid number.", attributeName));
      }
    }

    return length;
  }

  private boolean isLengthOutOfRange(Integer length) {
    return length != null && (length < MIN_ALLOWED_LENGTH || length > MAX_ALLOWED_LENGTH);
  }

  private String getLengthErrorMessage(String attributeName) {
    return format("The attribute \"%s\" must be between %s and %s", attributeName, MIN_ALLOWED_LENGTH, MAX_ALLOWED_LENGTH);
  }

}
