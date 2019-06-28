package org.symphonyoss.symphony.messageml.elements;

import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormElementNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

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
  private static final String MARKDOWN = "Date Selector";
  private static final String CLASS_ATTR = "class";

  public DateSelector(Element parent) {
    super(parent, MESSAGEML_TAG);
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
    out.printElement(PRESENTATIONML_TAG, null, presentationAttrs);
  }

  @Override
  public Node asMarkdown() {
    return new FormElementNode(MARKDOWN);
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
}
