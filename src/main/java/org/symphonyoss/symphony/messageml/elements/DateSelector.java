package org.symphonyoss.symphony.messageml.elements;

import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.DateSelectorNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import java.util.HashMap;
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
  private static final String REQUIRED_ATTR = "required";

  private static final String PRESENTATIONML_NAME_ATTR = "data-name";
  private static final String PRESENTATIONML_PLACEHOLDER_ATTR = "data-placeholder";
  private static final String PRESENTATIONML_REQUIRED_ATTR = "data-required";

  private static final String PRESENTATIONML_TAG = "div";
  private static final String MARKDOWN = "Date Selector";
  private static final String CLASS_ATTR = "class";

  public DateSelector(Element parent, FormatEnum messageFormat) {
    super(parent, MESSAGEML_TAG, messageFormat);
  }

  @Override
  public void buildAll(MessageMLParser parser, org.w3c.dom.Element element) throws InvalidInputException, ProcessingException {
    switch (getFormat()) {
      case MESSAGEML:
        super.buildAll(parser, element);
        break;
      case PRESENTATIONML:
        buildElementFromDiv(parser, element);
        this.validate();
        break;
      default:
        throw new InvalidInputException(String.format("Invalid message format for \"%s\" element", MESSAGEML_TAG));
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();
    
    assertAttributeNotBlank(NAME_ATTR);
    
    if(getAttribute(REQUIRED_ATTR) != null) {
      assertAttributeIsBoolean(REQUIRED_ATTR);
    }

    assertNoContent();
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    Map<String, String> presentationAttrs = buildDateSelectorInputAttributes();
    out.openElement(PRESENTATIONML_TAG, presentationAttrs);
    out.closeElement();
  }

  @Override
  public Node asMarkdown() {
    return new DateSelectorNode(getAttribute(PLACEHOLDER_ATTR));
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
        setAttribute(NAME_ATTR, getStringAttribute(item));
        break;
      case PLACEHOLDER_ATTR:
        setAttribute(PLACEHOLDER_ATTR, getStringAttribute(item));
        break;
      case REQUIRED_ATTR:
        setAttribute(REQUIRED_ATTR, getStringAttribute(item));
        break;
      case FORMNOVALIDATE_ATTR:
        setAttribute(FORMNOVALIDATE_ATTR, getStringAttribute(item));
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public String getPresentationMLTag() {
    return PRESENTATIONML_TAG;
  }

  @Override
  public void updateBiContext(BiContext context) {
    Map<String, Object> attributesMapBi = new HashMap<>();

    this.putOneIfPresent(attributesMapBi, BiFields.PLACEHOLDER.getValue(), PLACEHOLDER_ATTR);
    this.putOneIfPresent(attributesMapBi, BiFields.REQUIRED.getValue(), REQUIRED_ATTR);
    // no default values to check
    // no validations to check

    context.addItem(new BiItem(BiFields.DATE_SELECTOR.getValue(), attributesMapBi));
  }

  private Map<String, String> buildDateSelectorInputAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();

    presentationAttrs.put(CLASS_ATTR, MESSAGEML_TAG);
    presentationAttrs.put(PRESENTATIONML_NAME_ATTR, getAttribute(NAME_ATTR));

    if(getAttribute(PLACEHOLDER_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_PLACEHOLDER_ATTR, getAttribute(PLACEHOLDER_ATTR));
    }

    if(getAttribute(REQUIRED_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_REQUIRED_ATTR, getAttribute(REQUIRED_ATTR));
    }

    if(getAttribute(FORMNOVALIDATE_ATTR) != null) {
      presentationAttrs.put(FORMNOVALIDATE_PML_ATTR, getAttribute(FORMNOVALIDATE_ATTR));
    }

    return presentationAttrs;
  }

  void buildElementFromDiv(MessageMLParser parser, org.w3c.dom.Element element) throws InvalidInputException, ProcessingException {

    element.setAttribute(NAME_ATTR, element.getAttribute(PRESENTATIONML_NAME_ATTR));
    element.removeAttribute(PRESENTATIONML_NAME_ATTR);

    if (element.hasAttribute(PRESENTATIONML_PLACEHOLDER_ATTR)) {
      element.setAttribute(PLACEHOLDER_ATTR, element.getAttribute(PRESENTATIONML_PLACEHOLDER_ATTR));
      element.removeAttribute(PRESENTATIONML_PLACEHOLDER_ATTR);
    }

    if (element.hasAttribute(PRESENTATIONML_REQUIRED_ATTR)) {
      element.setAttribute(REQUIRED_ATTR, element.getAttribute(PRESENTATIONML_REQUIRED_ATTR));
      element.removeAttribute(PRESENTATIONML_REQUIRED_ATTR);
    }

    if (element.hasAttribute(FORMNOVALIDATE_PML_ATTR)) {
      element.setAttribute(FORMNOVALIDATE_ATTR, element.getAttribute(FORMNOVALIDATE_PML_ATTR));
      element.removeAttribute(FORMNOVALIDATE_PML_ATTR);
    }

    NamedNodeMap attributes = element.getAttributes();

    for (int i = 0; i < attributes.getLength(); i++) {
      buildAttribute(parser, attributes.item(i));
    }

    NodeList children = element.getChildNodes();

    for (int i = 0; i < children.getLength(); i++) {
      buildNode(parser, children.item(i));
    }

  }
}
