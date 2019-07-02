package org.symphonyoss.symphony.messageml.elements;

import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormElementNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Class representing a Checkbox inside a Form.
 * @author Cristiano Faustino
 * @since 05/29/2019
 */
public class Checkbox extends FormElement {
  public static final String MESSAGEML_TAG = "checkbox";
  public static final String PRESENTATIONML_INPUT_TYPE = "checkbox";
  public static final String PRESENTATIONML_DIV_CLASS = "checkbox-group";

  private static final String VALUE_ATTR = "value";
  private static final String CHECKED_ATTR = "checked";

  private static final String PRESENTATIONML_DIV_TAG = "div";
  private static final String PRESENTATIONML_CLASS_ATTR = "class";
  private static final String PRESENTATIONML_DEFAULT_CHECKBOX_VALUE = "on";
  private static final String PRESENTATIONML_LABEL_TAG = "label";
  private static final int PRESENTATIONML_DIV_NUMBER_OF_CHILDREN = 2;

  private static final String MARKDOWN = "Checkbox:";

  public Checkbox(Element parent, FormatEnum messageFormat) {
    super(parent, MESSAGEML_TAG, messageFormat);
  }

  @Override
  public void buildAll(MessageMLParser context, org.w3c.dom.Element element) throws InvalidInputException, ProcessingException {
    switch (getFormat()) {
      case MESSAGEML:
        super.buildAll(context, element);
        break;
      case PRESENTATIONML:
        if(INPUT_TAG.equals(element.getNodeName())) {
          buildCheckboxAttrFromInputTag(element);
        } else {
          buildElementFromGroupDiv(context, element);
        }
        this.validate();
        break;
      default:
        throw new InvalidInputException(String.format("Invalid message format for \"%s\" element", MESSAGEML_TAG));
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    if (getAttribute(NAME_ATTR) == null) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }

    if (getAttribute(CHECKED_ATTR) != null) {
      assertAttributeValue(CHECKED_ATTR, Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    if (!getChildren().isEmpty()) {
      assertContentModel(Arrays.asList(TextNode.class, Bold.class, Italic.class));
    }    
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
        setAttribute(NAME_ATTR, getStringAttribute(item));
        break;
      case VALUE_ATTR:
        setAttribute(VALUE_ATTR, getStringAttribute(item));
        break;
      case CHECKED_ATTR:
        setAttribute(CHECKED_ATTR, getStringAttribute(item));
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    Map<String, String> presentationAttrs = buildCheckboxInputAttributes();
    
    if (getChildren().isEmpty()) {
      out.printElement(INPUT_TAG, presentationAttrs);
    }
    else {
      out.openElement(PRESENTATIONML_DIV_TAG, PRESENTATIONML_CLASS_ATTR, PRESENTATIONML_DIV_CLASS);
      
      out.printElement(INPUT_TAG, presentationAttrs);

      out.openElement(PRESENTATIONML_LABEL_TAG);
      for (Element child : getChildren()) {
        child.asPresentationML(out);
      }
      out.closeElement(); // Closing label

      out.closeElement(); // Closing div
    }
  }

  @Override
  public Node asMarkdown() {
    return new FormElementNode(MARKDOWN, getAttribute(NAME_ATTR));
  }

  private void buildElementFromGroupDiv(MessageMLParser context, org.w3c.dom.Element element) throws InvalidInputException, ProcessingException {
    NodeList children = element.getChildNodes();

    if (children.getLength() != PRESENTATIONML_DIV_NUMBER_OF_CHILDREN) {
      throw new InvalidInputException(String.format("Invalid PresentationML for the \"%s\" element", MESSAGEML_TAG));
    }

    String firstNodeName = "";
    for (int i = 0; i < PRESENTATIONML_DIV_NUMBER_OF_CHILDREN; i++) {
      if(firstNodeName.equals(children.item(i).getNodeName())) {
        throw new InvalidInputException(String.format("Invalid PresentationML for the \"%s\" element", MESSAGEML_TAG));
      }

      switch (children.item(i).getNodeName()) {
        case INPUT_TAG:
          buildCheckboxAttrFromInputTag(children.item(i));
          firstNodeName = INPUT_TAG;
          break;
        case PRESENTATIONML_LABEL_TAG:
          buildCheckboxTextFromLabelTag(context, children.item(i));
          firstNodeName = PRESENTATIONML_LABEL_TAG;
          break;
        default:
          throw new InvalidInputException(String.format("Invalid PresentationML for the \"%s\" element", MESSAGEML_TAG));
      }
    }
  }

  private void buildCheckboxTextFromLabelTag(MessageMLParser context, org.w3c.dom.Node labelElement) throws InvalidInputException, ProcessingException {
    NodeList childNodes = labelElement.getChildNodes();
    if(childNodes == null || childNodes.getLength() <= 0) {
      throw new InvalidInputException(String.format("Invalid PresentationML for the \"%s\" element", MESSAGEML_TAG));
    }

    for (int i = 0; i < childNodes.getLength(); i++) {
      buildNode(context, childNodes.item(i));
    }
  }

  private void buildCheckboxAttrFromInputTag(org.w3c.dom.Node inputElement) throws InvalidInputException {
    NamedNodeMap inputAttributes = inputElement.getAttributes();
    inputAttributes.removeNamedItem(TYPE_ATTR);

    for (int i = 0; i < inputAttributes.getLength(); i++) {
      buildAttribute(inputAttributes.item(i));
    }
  }

  private Map<String, String> buildCheckboxInputAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();
    presentationAttrs.put(TYPE_ATTR, PRESENTATIONML_INPUT_TYPE);
    presentationAttrs.put(NAME_ATTR, getAttribute(NAME_ATTR));

    if (getAttribute(CHECKED_ATTR) != null) {
      presentationAttrs.put(CHECKED_ATTR, getAttribute(CHECKED_ATTR));
    }

    if (getAttribute(VALUE_ATTR) != null) {
      presentationAttrs.put(VALUE_ATTR, getAttribute(VALUE_ATTR));
    } else {
      presentationAttrs.put(VALUE_ATTR, PRESENTATIONML_DEFAULT_CHECKBOX_VALUE);
    }
    return presentationAttrs;
  }
}
