package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormElementNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Class representing a Password Field inside a Form.
 * @author Sandro Ribeiro
 * @since 06/13/2019
 */
public class Password extends FormElement {

  public static final String MESSAGEML_TAG = "password";
  public static final String PRESENTATIONML_INPUT_TYPE = "password";

  private static final String MINLENGTH_ATTR = "minlength";
  private static final String MAXLENGTH_ATTR = "maxlength";
  private static final String REQUIRED_ATTR = "required";
  private static final String PLACEHOLDER_ATTR = "placeholder";
  private static final String VALUE_ATTR = "value";

  private static final List<String> VALID_VALUES_FOR_REQUIRED_ATTR = Arrays.asList("true", "false");

  private final static String MARKDOWN = "Password Field";

  private static final String PRESENTATIONML_INPUT_TAG = "input";
  private static final String PRESENTATIONML_TYPE_ATTR = "type";

  private static final int MIN_ALLOWED_LENGTH = 1;
  private static final int MAX_ALLOWED_LENGTH = 128;

  public Password(Element parent, FormatEnum format) {
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

    validateMinAndMaxLengths();
    assertContentModel(Collections.singleton(TextNode.class));
  }

  @Override
  public void buildAll(MessageMLParser context, org.w3c.dom.Element element)
          throws InvalidInputException, ProcessingException {
    switch (getFormat()) {
      case MESSAGEML:
        super.buildAll(context, element);
        break;
      case PRESENTATIONML:
        this.buildAllForPresentationML(element);
        break;
    }
  }

  private void buildAllForPresentationML(org.w3c.dom.Element element)
      throws InvalidInputException {
    NamedNodeMap attr = element.getAttributes();
    NodeList children = element.getChildNodes();

    if (children != null && children.getLength() > 0) {
      throw new InvalidInputException(
          "Element \"" + this.getMessageMLTag() + "\" may not have child elements or text content");
    }

    for (int i = 0; i < attr.getLength(); i++) {
      buildAttributeForPresentationML(attr.item(i));
    }
  }

  private void buildAttributeForPresentationML(org.w3c.dom.Node item) throws InvalidInputException {
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
      case VALUE_ATTR:
        addChild(new TextNode(this, getStringAttribute(item)));
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
                + "\" is not allowed in \"" + getMessageMLTag() + "\"");
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
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    Map<String, String> presentationAttrs = buildTextFieldInputAttributes();
    out.printElement(PRESENTATIONML_INPUT_TAG, presentationAttrs);
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    if (getAttribute(PLACEHOLDER_ATTR) != null) {
      return new FormElementNode(MARKDOWN, ":" + getAttribute(PLACEHOLDER_ATTR));
    } else {
      return new FormElementNode(MARKDOWN);
    }
  }

  private Map<String, String> buildTextFieldInputAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();

    presentationAttrs.put(PRESENTATIONML_TYPE_ATTR, PRESENTATIONML_INPUT_TYPE);
    presentationAttrs.put(NAME_ATTR, getAttribute(NAME_ATTR));

    if (getAttribute(PLACEHOLDER_ATTR) != null) {
      presentationAttrs.put(PLACEHOLDER_ATTR, getAttribute(PLACEHOLDER_ATTR));
    }

    if (getAttribute(REQUIRED_ATTR) != null) {
      presentationAttrs.put(REQUIRED_ATTR, getAttribute(REQUIRED_ATTR));
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
    int maxLength = getLengthAttributeAsInt(MAXLENGTH_ATTR);
    if (isLengthIsOutOfRange(maxLength)) {
      throw new InvalidInputException(getLengthErrorMessage(MAXLENGTH_ATTR));
    }

    int minLength = getLengthAttributeAsInt(MINLENGTH_ATTR);
    if (isLengthIsOutOfRange(minLength)) {
      throw new InvalidInputException(getLengthErrorMessage(MINLENGTH_ATTR));
    }

    if (minLength > maxLength) {
      throw new InvalidInputException("The attribute \"minlength\" must be lower than the \"maxlength\" attribute");
    }
  }

  private int getLengthAttributeAsInt(String attributeName) throws InvalidInputException {
    int length = 1;

    if (getAttribute(attributeName) != null) {
      try {
        length = Integer.parseInt(getAttribute(attributeName));
      } catch (NumberFormatException e) {
        throw new InvalidInputException(getLengthErrorMessage(attributeName));
      }
    }

    return length;
  }

  private boolean isLengthIsOutOfRange(int length) {
    return (length < MIN_ALLOWED_LENGTH || length > MAX_ALLOWED_LENGTH);
  }

  private String getLengthErrorMessage(String attributeName) {
    return String.format("The attribute \"" + attributeName + "\" must be between %s and %s", MIN_ALLOWED_LENGTH,
        MAX_ALLOWED_LENGTH);
  }

}
