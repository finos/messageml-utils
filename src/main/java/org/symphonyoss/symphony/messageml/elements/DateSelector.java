package org.symphonyoss.symphony.messageml.elements;

import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormElementNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Class representing a date-selector element inside a Symphony Elements form.
 *
 * @author Cristiano Faustino
 * @since 06/11/2019
 */
public class DateSelector extends FormElement {
  public static final String MESSAGEML_TAG = "date-selector";
  
  private static final String PLACEHOLDER_ATTR = "placeholder";
  
  private static final String PRESENTATIONML_NAME_ATTR = "data-name";
  private static final String PRESENTATIONML_PLACEHOLDER_ATTR = "data-placeholder";
  
  private static final String PRESENTATIONML_TAG = "div";
  private static final String MARKDOWN = "Date Selector:";
  private static final String CLASS_ATTR = "class";

  public DateSelector(Element parent, FormatEnum messageFormat) {
    super(parent, MESSAGEML_TAG, messageFormat);
  }

  @Override
  public void buildAll(MessageMLParser context, org.w3c.dom.Element element) throws InvalidInputException, ProcessingException {
    switch (getFormat()) {
      case MESSAGEML:
        super.buildAll(context, element);
        break;
      case PRESENTATIONML:
        buildElementFromDiv(context, element);
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

    assertNoContent();
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    Map<String, String> presentationAttrs = buildDateSelectorInputAttributes();
    out.printElement(PRESENTATIONML_TAG, presentationAttrs);
  }

  @Override
  public Node asMarkdown() {
    return new FormElementNode(MARKDOWN, getAttribute(NAME_ATTR));
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
        setAttribute(NAME_ATTR, getStringAttribute(item));
        break;
      case PLACEHOLDER_ATTR:
        setAttribute(PLACEHOLDER_ATTR, getStringAttribute(item));
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  private Map<String, String> buildDateSelectorInputAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();

    presentationAttrs.put(CLASS_ATTR, MESSAGEML_TAG);
    presentationAttrs.put(PRESENTATIONML_NAME_ATTR, getAttribute(NAME_ATTR));

    if(getAttribute(PLACEHOLDER_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_PLACEHOLDER_ATTR, getAttribute(PLACEHOLDER_ATTR));
    }

    return presentationAttrs;
  }

  void buildElementFromDiv(MessageMLParser context, org.w3c.dom.Element element) throws InvalidInputException, ProcessingException {

    if (!"".equals(element.getAttribute(PRESENTATIONML_NAME_ATTR))) {
      element.setAttribute(NAME_ATTR, element.getAttribute(PRESENTATIONML_NAME_ATTR));
      element.removeAttribute(PRESENTATIONML_NAME_ATTR);
    }

    if (!"".equals(element.getAttribute(PRESENTATIONML_PLACEHOLDER_ATTR))) {
      element.setAttribute(PLACEHOLDER_ATTR, element.getAttribute(PRESENTATIONML_PLACEHOLDER_ATTR));
      element.removeAttribute(PRESENTATIONML_PLACEHOLDER_ATTR);
    }

    NamedNodeMap attributes = element.getAttributes();

    for (int i = 0; i < attributes.getLength(); i++) {
      buildAttribute(attributes.item(i));
    }

    NodeList children = element.getChildNodes();

    for (int i = 0; i < children.getLength(); i++) {
      buildNode(context, children.item(i));
    }

  }
}
