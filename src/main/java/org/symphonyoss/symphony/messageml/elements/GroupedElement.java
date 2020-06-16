package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author enrico.molino (15/06/2020)
 */
public abstract class GroupedElement extends FormElement {

  private static final String PRESENTATIONML_DIV_TAG = "div";
  private static final String PRESENTATIONML_CLASS_ATTR = "class";
  private static final int PRESENTATIONML_DIV_NUMBER_OF_CHILDREN = 2;
  private static final String PRESENTATIONML_DEFAULT_VALUE = "on";
  protected static final String PRESENTATIONML_LABEL_TAG = "label";
  protected static final String VALUE_ATTR = "value";
  protected static final String CHECKED_ATTR = "checked";

  public GroupedElement(Element parent, String messageMLTag,
      FormatEnum format) {
    super(parent, messageMLTag, format);
  }

  @Override
  public void buildAll(MessageMLParser parser, org.w3c.dom.Element element) throws InvalidInputException,
      ProcessingException {
    switch (getFormat()) {
      case MESSAGEML:
        super.buildAll(parser, element);
        break;
      case PRESENTATIONML:
        if(INPUT_TAG.equals(element.getNodeName())) {
          buildElementAttrFromInputTag(parser, element);
        } else {
          buildElementFromGroupDiv(parser, element);
        }
        this.validate();
        break;
      default:
        throw new InvalidInputException(String.format("Invalid message format for \"%s\" element", getMessageMLTag()));
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    Map<String, String> presentationAttrs = buildGroupedElementInputAttributes();

    if (getChildren().isEmpty()) {
      out.printElement(INPUT_TAG, presentationAttrs);
    }
    else {
      out.openElement(PRESENTATIONML_DIV_TAG, PRESENTATIONML_CLASS_ATTR, getPresentationMLDivClass());

      out.printElement(INPUT_TAG, presentationAttrs);

      out.openElement(PRESENTATIONML_LABEL_TAG);
      for (Element child : getChildren()) {
        child.asPresentationML(out, context);
      }
      out.closeElement(); // Closing label

      out.closeElement(); // Closing div
    }
  }

  protected void buildElementFromGroupDiv(MessageMLParser parser, org.w3c.dom.Element element)
      throws
      InvalidInputException, ProcessingException {
    NodeList children = element.getChildNodes();
    Integer numberOfNonTextChildrenNodes = countNonTextNodesInNodeList(children);

    if (numberOfNonTextChildrenNodes != PRESENTATIONML_DIV_NUMBER_OF_CHILDREN) {
      throw new InvalidInputException(String.format("Invalid PresentationML for the \"%s\" element", getMessageMLTag()));
    }

    String firstNodeName = "";
    for (int i = 0; i < children.getLength(); i++) {
      if(firstNodeName.equals(children.item(i).getNodeName())) {
        throw new InvalidInputException(String.format("Invalid PresentationML for the \"%s\" element", getMessageMLTag()));
      }

      switch (children.item(i).getNodeName()) {
        case INPUT_TAG:
          buildElementAttrFromInputTag(parser, children.item(i));
          firstNodeName = INPUT_TAG;
          break;
        case PRESENTATIONML_LABEL_TAG:
          buildTextFromLabelTag(parser, children.item(i));
          firstNodeName = PRESENTATIONML_LABEL_TAG;
          break;
        case "#text":
          break;
        default:
          throw new InvalidInputException(String.format("Invalid PresentationML for the \"%s\" element", getMessageMLTag()));
      }
    }
  }

  private void buildTextFromLabelTag(MessageMLParser context, org.w3c.dom.Node labelElement) throws InvalidInputException, ProcessingException {
    NodeList childNodes = labelElement.getChildNodes();
    if(childNodes == null || childNodes.getLength() <= 0) {
      throw new InvalidInputException(String.format("Invalid PresentationML for the \"%s\" element", getMessageMLTag()));
    }

    for (int i = 0; i < childNodes.getLength(); i++) {
      buildNode(context, childNodes.item(i));
    }
  }

  protected void buildElementAttrFromInputTag(MessageMLParser parser, org.w3c.dom.Node inputElement)
      throws InvalidInputException {
    NamedNodeMap inputAttributes = inputElement.getAttributes();
    inputAttributes.removeNamedItem(TYPE_ATTR);

    for (int i = 0; i < inputAttributes.getLength(); i++) {
      buildAttribute(parser, inputAttributes.item(i));
    }
  }

  protected Map<String, String> buildGroupedElementInputAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();
    presentationAttrs.put(TYPE_ATTR, getPresentationMLInputType());
    presentationAttrs.put(NAME_ATTR, getAttribute(NAME_ATTR));

    if (getAttribute(CHECKED_ATTR) != null) {
      presentationAttrs.put(CHECKED_ATTR, getAttribute(CHECKED_ATTR));
    }

    if (getAttribute(VALUE_ATTR) != null) {
      presentationAttrs.put(VALUE_ATTR, getAttribute(VALUE_ATTR));
    } else {
      presentationAttrs.put(VALUE_ATTR, PRESENTATIONML_DEFAULT_VALUE);
    }
    return presentationAttrs;
  }

  protected abstract String getPresentationMLInputType();

  protected abstract String getPresentationMLDivClass();
}
