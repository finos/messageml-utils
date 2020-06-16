package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.OptionNode;
import org.w3c.dom.Node;

import java.util.Arrays;
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
  private static final String SELECTED_ATTR = "selected";

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

    if (getAttribute(SELECTED_ATTR) != null) {
      assertAttributeValue(SELECTED_ATTR, Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    assertParent(Collections.singleton(Select.class));
    assertContentModel(Collections.singleton(TextNode.class));
    assertContainsChildOfType(Collections.singleton(TextNode.class));
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case VALUE_ATTR:
        setAttribute(VALUE_ATTR, getStringAttribute(item));
        break;
      case SELECTED_ATTR:
        setAttribute(SELECTED_ATTR, getStringAttribute(item));
        break;
      default:
        throwInvalidInputException(item);
    }
  }
}
