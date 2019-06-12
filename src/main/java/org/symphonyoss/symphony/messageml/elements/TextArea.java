package org.symphonyoss.symphony.messageml.elements;

import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormElementNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class TextArea extends FormElement {

  public static final String MESSAGEML_TAG = "textarea";

  private static final String PLACEHOLDER_ATTR = "placeholder";
  private static final String REQUIRED_ATTR = "required";
  private static final List<String>
      VALID_VALUES_FOR_REQUIRED_ATTR = Arrays.asList("true", "false");

  private final static String MARKDOWN = "Text Area";

  public TextArea(Element parent) {
    super(parent, MESSAGEML_TAG);
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
    Map<String, String> presentationAttrs = buildTextAreaAttributes();
    out.openElement(getMessageMLTag(), presentationAttrs);
    for (Element child : getChildren()) {
      child.asPresentationML(out);
    }
    out.closeElement();
  }

  @Override
  public Node asMarkdown() {
    if (getAttribute(PLACEHOLDER_ATTR) != null) {
      return new FormElementNode(MARKDOWN, ":" + getAttribute(PLACEHOLDER_ATTR));
    } else {
      return new FormElementNode(MARKDOWN, "");
    }
  }

  // Inner content should be not rendered in the Markdown output
  @Override
  public void buildMarkdown(Node parent) {
  }

  private Map<String, String> buildTextAreaAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();

    presentationAttrs.put(NAME_ATTR, getAttribute(NAME_ATTR));

    if (getAttribute(PLACEHOLDER_ATTR) != null) {
      presentationAttrs.put(PLACEHOLDER_ATTR, getAttribute(PLACEHOLDER_ATTR));
    }

    if (getAttribute(REQUIRED_ATTR) != null) {
      presentationAttrs.put(REQUIRED_ATTR, getAttribute(REQUIRED_ATTR));
    }

    return presentationAttrs;
  }
}
