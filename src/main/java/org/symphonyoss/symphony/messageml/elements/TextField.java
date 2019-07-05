package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormElementNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import java.util.*;


/**
 * Class representing a Text Field inside a Form.
 * @author Lucas Macedo
 * @since 06/07/2019
 */
public class TextField extends FormElement {

  public static final String MESSAGEML_TAG = "text-field";
  public static final String PRESENTATIONML_INPUT_TYPE = "text";

  private static final String REQUIRED_ATTR = "required";
  private static final String PLACEHOLDER_ATTR = "placeholder";
  private static final String VALUE_ATTR = "value";
  private static final Set<String> VALID_VALUES_FOR_REQUIRED_ATTR = new HashSet<>(Arrays.asList("true", "false"));

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
      assertAttributeValue(REQUIRED_ATTR, VALID_VALUES_FOR_REQUIRED_ATTR);
    }

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
      case VALUE_ATTR:
        addChild(new TextNode(this, getStringAttribute(item)));
        break;
      case REQUIRED_ATTR:
        setAttribute(REQUIRED_ATTR, getStringAttribute(item));
        break;
      case PLACEHOLDER_ATTR:
        setAttribute(PLACEHOLDER_ATTR, getStringAttribute(item));
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
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    Map<String, String> presentationAttrs = buildTextFieldInputAttributes();
    out.printElement(INPUT_TAG, presentationAttrs);
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

    presentationAttrs.put(TYPE_ATTR, PRESENTATIONML_INPUT_TYPE);
    presentationAttrs.put(NAME_ATTR, getAttribute(NAME_ATTR));

    if (getAttribute(PLACEHOLDER_ATTR) != null) {
      presentationAttrs.put(PLACEHOLDER_ATTR, getAttribute(PLACEHOLDER_ATTR));
    }

    if (getAttribute(REQUIRED_ATTR) != null) {
      presentationAttrs.put(REQUIRED_ATTR, getAttribute(REQUIRED_ATTR));
    }

    if (getChildren() != null && getChildren().size() == 1) {
      presentationAttrs.put(VALUE_ATTR, getChildren().get(0).asText());
    }

    return presentationAttrs;
  }


}
