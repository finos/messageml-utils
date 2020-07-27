package org.symphonyoss.symphony.messageml.elements;

import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.PersonSelectorNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * Class representing a person selector inside a Symphony Elements form.
 *
 * @author Cristiano Faustino
 * @since 06/11/2019
 */
public class PersonSelector extends FormElement implements LabelableElement, TooltipableElement {
  public static final String MESSAGEML_TAG = "person-selector";

  private static final String PLACEHOLDER_ATTR = "placeholder";
  private static final String REQUIRED_ATTR = "required";
  private static final Set<String> VALID_VALUES_FOR_REQUIRED_ATTR = new HashSet<>(Arrays.asList("true", "false"));

  private static final String PRESENTATIONML_TAG = "div";
  private static final String PRESENTATIONML_PLACEHOLDER_ATTR = "data-placeholder";
  private static final String PRESENTATIONML_REQUIRED_ATTR = "data-required";

  private final static String MARKDOWN = "Person Selector";
  private static final String CLASS_ATTR = "class";
  private static final String PRESENTATIONML_NAME_ATTR = "data-name";

  public PersonSelector(Element parent, FormatEnum messageFormat) {
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

    if (getAttribute(NAME_ATTR) == null) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }

    if(getAttribute(REQUIRED_ATTR) != null) {
      assertAttributeValue(REQUIRED_ATTR, VALID_VALUES_FOR_REQUIRED_ATTR);
    }

    assertNoContent();
    assertAttributeNotBlank(NAME_ATTR);
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    Map<String, String> presentationAttrs = buildPersonSelectorInputAttributes();
    if(isSplittable()){
      // open div + adding splittable elements
      presentationAttrs.put("id", splittableAsPresentationML(out, context));
      // render element
      innerAsPresentationML(out, presentationAttrs);
      // close div
      out.closeElement();
    } else {
      innerAsPresentationML(out, presentationAttrs);
    }
  }

  private void innerAsPresentationML(XmlPrintStream out, Map<String, String> presentationAttrs){
    out.openElement(PRESENTATIONML_TAG, presentationAttrs);
    out.closeElement();
  }

  @Override
  public Node asMarkdown() {
    return new PersonSelectorNode(getAttribute(PLACEHOLDER_ATTR), getAttribute(LABEL), getAttribute(TITLE));
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case PLACEHOLDER_ATTR:
      case REQUIRED_ATTR:
      case LABEL:
      case TITLE:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case ID_ATTR:
        if(format != FormatEnum.PRESENTATIONML){
          throwInvalidInputException(item);
        }
        fillAttributes(parser, item);
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  private Map<String, String> buildPersonSelectorInputAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();

    presentationAttrs.put(CLASS_ATTR, MESSAGEML_TAG);
    presentationAttrs.put(PRESENTATIONML_NAME_ATTR, getAttribute(NAME_ATTR));

    if(getAttribute(PLACEHOLDER_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_PLACEHOLDER_ATTR, getAttribute(PLACEHOLDER_ATTR));
    }

    if(getAttribute(REQUIRED_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_REQUIRED_ATTR, getAttribute(REQUIRED_ATTR));
    }

    return presentationAttrs;
  }

  private void buildElementFromDiv(MessageMLParser parser, org.w3c.dom.Element element) throws InvalidInputException, ProcessingException {

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

    NamedNodeMap attributes = element.getAttributes();

    for (int i = 0; i < attributes.getLength(); i++) {
      buildAttribute(parser, attributes.item(i));
    }

    NodeList children = element.getChildNodes();

    for (int i = 0; i < children.getLength(); i++) {
      buildNode(parser, children.item(i));
    }

  }

  @Override
  public String getPresentationMLTag() {
    return PRESENTATIONML_TAG;
  }

  @Override
  public String getElementId(){
    return MESSAGEML_TAG;
  }
}
