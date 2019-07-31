package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormNode;

import java.util.Collections;

/**
 * Class representing a Symphony Elements form
 *
 * @author lumoura
 * @since 03/21/19
 */
public class Form extends Element {
  public static final String MESSAGEML_TAG = "form";
  private static final String ID_ATTR = "id";
  
  public Form(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();
    assertNotParentAtAnyLevel(Collections.singletonList(this.getClass()));

    if (getAttribute(ID_ATTR) == null) {
      throw new InvalidInputException("The attribute \"id\" is required");
    }

    assertAttributeNotBlank(ID_ATTR);
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
  public org.commonmark.node.Node asMarkdown() {
    return new FormNode();
  }
}