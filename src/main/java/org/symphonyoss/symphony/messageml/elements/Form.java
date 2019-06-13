package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Class representing a Symphony Elements form
 *
 * @author lumoura
 * @since 03/21/19
 */
public class Form extends Element {
  public static final String MESSAGEML_TAG = "form";
  private static final String ID_ATTR = "id";
  
  private static final String PRESENTATIONML_TAG = "form";

  public Form(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    if (getAttribute(ID_ATTR) == null) {
      throw new InvalidInputException("The attribute \"id\" is required");
    }
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ID_ATTR:
        setAttribute(ID_ATTR, getStringAttribute(item));
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");  
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();
    presentationAttrs.put(ID_ATTR, getAttribute(ID_ATTR));
    
    out.openElement(PRESENTATIONML_TAG, presentationAttrs);
    for (Element child : getChildren()) {
      child.asPresentationML(out);
    }
    out.closeElement();
  }
  
  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new FormNode();
  }
}