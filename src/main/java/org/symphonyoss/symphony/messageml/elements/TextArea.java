package org.symphonyoss.symphony.messageml.elements;

import static java.lang.String.format;

import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.TextAreaNode;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Class representing a Text Area inside a Form.
 * @author Sandro Ribeiro
 * @since 06/12/2019
 */
public class TextArea extends FormElement implements RegexElement, LabelableElement, TooltipableElement {

  public static final String MESSAGEML_TAG = "textarea";

  private static final String MINLENGTH_ATTR = "minlength";
  private static final String MAXLENGTH_ATTR = "maxlength";
  private Integer MIN_ALLOWED_LENGTH = 0;
  private Integer MAX_ALLOWED_LENGTH = 10000;

  private static final String PLACEHOLDER_ATTR = "placeholder";
  private static final String REQUIRED_ATTR = "required";
  private static final List<String> VALID_VALUES_FOR_REQUIRED_ATTR = Arrays.asList("true", "false");

  public TextArea(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    if (getAttribute(NAME_ATTR) == null) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }

    if (getAttribute(REQUIRED_ATTR) != null) {
      assertAttributeValue(REQUIRED_ATTR, VALID_VALUES_FOR_REQUIRED_ATTR);
    }

    assertAttributeNotBlank(NAME_ATTR);
    assertContentModel(Collections.singleton(TextNode.class));
    validateMinAndMaxLengths();

  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case REQUIRED_ATTR:
      case PLACEHOLDER_ATTR:
      case MINLENGTH_ATTR:
      case MAXLENGTH_ATTR:
      case PATTERN_ATTR:
      case LABEL:
      case TITLE:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case PATTERN_ERROR_MESSAGE_ATTR:
        if(this.format != FormatEnum.MESSAGEML){
          throwInvalidInputException(item);
        }
        setAttribute(PATTERN_ERROR_MESSAGE_ATTR, getStringAttribute(item));
        break;
      case PRESENTATIONML_PATTERN_ERROR_MESSAGE_ATTR:
        if(this.format != FormatEnum.PRESENTATIONML){
          throwInvalidInputException(item);
        }
        setAttribute(PRESENTATIONML_PATTERN_ERROR_MESSAGE_ATTR, getStringAttribute(item));
        break;
      case ID_ATTR:
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
  public Node asMarkdown() {
      return new TextAreaNode(getAttribute(PLACEHOLDER_ATTR), hasExactNumberOfChildren(1) ? getChild(0).asText() : null);
  }

  @Override
  public String getElementId(){
    return MESSAGEML_TAG;
  }

  /**
   * This method checks if the values assigned to minlength and maxlength attributes
   * are valid. If there is an initial value in the textarea it also checks if the latter is
   * between the range given
   *
   * @throws InvalidInputException when the attributes value are not valid or the input is not in range
   */
  private void validateMinAndMaxLengths() throws InvalidInputException {
    Integer maxLength = getAttributeAsInteger(MAXLENGTH_ATTR);
    if (isLengthOutOfPossibleRange(maxLength)) {
      throw new InvalidInputException(getLengthErrorMessage(MAXLENGTH_ATTR));
    }

    Integer minLength = getAttributeAsInteger(MINLENGTH_ATTR);
    if (isLengthOutOfPossibleRange(minLength)) {
      throw new InvalidInputException(getLengthErrorMessage(MINLENGTH_ATTR));
    }

    minLength = getDefaultValueIfCurrentIsNull(minLength, MIN_ALLOWED_LENGTH);
    maxLength = getDefaultValueIfCurrentIsNull(maxLength, MAX_ALLOWED_LENGTH);

    if (minLength > maxLength) {
      throw new InvalidInputException("The attribute \"minlength\" must be lower than the \"maxlength\" attribute");
    }
    validateInitialValueIfFound(maxLength, minLength);
  }

  private void validateInitialValueIfFound(Integer maxLength, Integer minLength) throws InvalidInputException {
    if (getChildren() != null && getChildren().size() == 1 && getChild(0) instanceof TextNode) {
      String initialValue = ((TextNode) getChild(0)).getText();
      if (initialValue.length() < minLength || initialValue.length() > maxLength) {
        throw new InvalidInputException(String.format(
                "The length of this textarea's initial value must be between %s and %s", minLength, maxLength));
      }
    }
  }

  private boolean isLengthOutOfPossibleRange(Integer length) {
    return length != null && (length < MIN_ALLOWED_LENGTH || length > MAX_ALLOWED_LENGTH);
  }


  private Integer getAttributeAsInteger(String attributeName) throws InvalidInputException{
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

  private Integer getDefaultValueIfCurrentIsNull(Integer currentValue, Integer defaultValue) {
    return currentValue == null ? defaultValue : currentValue;
  }

  private String getLengthErrorMessage(String attributeName) {
    return format("The attribute \"%s\" must be between %s and %s", attributeName, MIN_ALLOWED_LENGTH, MAX_ALLOWED_LENGTH);
  }

}
