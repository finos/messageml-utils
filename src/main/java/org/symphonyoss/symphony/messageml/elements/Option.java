package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.OptionNode;

import java.util.Collections;

/**
 * Class representing a Symphony Elements option
 *
 * @author lumoura
 * @since 03/22/19
 */
public class Option extends FormElement {
  public static final String MESSAGEML_TAG = "option";
  private static final String VALUE_ATTR = "value";

  public Option(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new OptionNode();
  }

  @Override
  public void validate() throws InvalidInputException {
    if (getAttribute(VALUE_ATTR) == null) {
      throw new InvalidInputException("The attribute \"value\" is required");
    }

    assertParent(Collections.singleton(Select.class));
    assertContentModel(Collections.singleton(TextNode.class));
    assertContainsElement(TextNode.class);
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
}