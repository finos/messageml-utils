package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.OptionNode;

import java.util.Collections;

/**
 * Class representing a Symphony Elements option
 *
 * @author lumoura
 * @since 03/22/19
 */
public class Option extends Element {

  public static final String MESSAGEML_TAG = "option";
  public static final String VALUE_ATTR = "value";

  public Option(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    if (item.getNodeName().equals(VALUE_ATTR)) {
      setAttribute(VALUE_ATTR, getStringAttribute(item));
    } else {
      throw new InvalidInputException("Attribute \"" + item.getNodeName()
              + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new OptionNode();
  }


  @Override
  public void validate() throws InvalidInputException {
    if (this.getParent().getClass() != Select.class) {
      throw new InvalidInputException("An \"" + getMessageMLTag() + "\" element can only be a child of a \"" +
              "select\" element");
    }
    if (getAttribute(VALUE_ATTR) == null) {
      throw new InvalidInputException("The attribute \"value\" is required");
    }
    assertContentModel(Collections.<Class<? extends Element>>singleton(TextNode.class));
  }
}