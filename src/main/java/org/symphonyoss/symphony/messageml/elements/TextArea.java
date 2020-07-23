package org.symphonyoss.symphony.messageml.elements;

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
public class TextArea extends FormElement implements RegexElement, LabelableElement, TooltipableElement, LimitedInputLengthElement {

  public static final String MESSAGEML_TAG = "textarea";

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
  public void buildAll(MessageMLParser parser, org.w3c.dom.Element element)
          throws InvalidInputException, ProcessingException {
    switch (getFormat()) {
      case MESSAGEML:
        super.buildAll(parser, element);
        break;
      case PRESENTATIONML:
        this.buildAllFromPresentationML(parser, element);
        break;
    }
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
  public Map<String, String> getOtherAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();

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

    return presentationAttrs;
  }

  @Override
  public Node asMarkdown() {
      return new TextAreaNode(getAttribute(PLACEHOLDER_ATTR), hasExactNumberOfChildren(1) ? getChild(0).asText() : null);
  }

  @Override
  public String getElementId(){
    return MESSAGEML_TAG;
  }

  @Override
  public String getElementInitialValue() {
    return ((TextNode) getChild(0)).getText();
  }

  @Override
  public boolean hasElementInitialValue() {
    return getChildren() != null && getChildren().size() == 1 && getChild(0) instanceof TextNode;
  }

  @Override
  public String getAttributeValue(String attributeName){
    return getAttribute(attributeName);
  }

  @Override
  public String getElementType(){ return MESSAGEML_TAG; }

  private void buildAllFromPresentationML(MessageMLParser parser, org.w3c.dom.Element element)
          throws InvalidInputException {
    NamedNodeMap attr = element.getAttributes();
    NodeList children = element.getChildNodes();

    if (children != null && children.getLength() > 0) {
      throw new InvalidInputException(
              "Element \"" + this.getMessageMLTag() + "\" may not have child elements or text content");
    }

    for (int i = 0; i < attr.getLength(); i++) {

      buildAttributeFromPresentationML(parser, attr.item(i));
    }
  }

  private void buildAttributeFromPresentationML(MessageMLParser parser, org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case REQUIRED_ATTR:
      case PLACEHOLDER_ATTR:
      case MINLENGTH_ATTR:
      case MAXLENGTH_ATTR:
      case LABEL:
      case PATTERN_ATTR:
      case PRESENTATIONML_PATTERN_ERROR_MESSAGE_ATTR:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case ID_ATTR:
        fillAttributes(parser, item);
        break;
      default:
        throwInvalidInputException(item);
    }
  }

}
