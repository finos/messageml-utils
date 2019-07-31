package org.symphonyoss.symphony.messageml.elements;

import org.apache.commons.lang3.StringUtils;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.ButtonNode;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;


/**
 * Class representing a Symphony Elements button
 *
 * @author lumoura
 * @since 03/21/19
 */
public class Button extends FormElement {
  public static final String MESSAGEML_TAG = "button";

  private static final Set<String> VALID_CLASSES = new HashSet<>(Arrays.asList("primary", "secondary",
          "primary-destructive", "secondary-destructive"));
  private static final Set<String> VALID_TYPES = new HashSet<>(Arrays.asList("action", "reset"));

  public Button(Element parent) {
    super(parent, MESSAGEML_TAG);
    setAttribute(TYPE_ATTR, "action");
  }

  @Override
  public void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
        setAttribute(NAME_ATTR, getStringAttribute(item).toLowerCase());
        break;

      case TYPE_ATTR:
        setAttribute(TYPE_ATTR, getStringAttribute(item).toLowerCase());
        break;

      case CLASS_ATTR:
        setAttribute(CLASS_ATTR, getStringAttribute(item).toLowerCase());
        break;

      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
                + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new ButtonNode();
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    String type = getAttribute(TYPE_ATTR);
    String name = getAttribute(NAME_ATTR);
    String clazz = getAttribute(CLASS_ATTR);

    if (!VALID_TYPES.contains(type)) {
      throw new InvalidInputException("Attribute \"type\" must be \"action\" or \"reset\"");
    }
    if (clazz != null && !VALID_CLASSES.contains(clazz)) {
      throw new InvalidInputException("Attribute \"class\" must be \"primary\", \"secondary\", " +
              "\"primary-destructive\" or \"secondary-destructive\"");
    }
    if (type.equals("action") && StringUtils.isBlank(name)) {
      throw new InvalidInputException("Attribute \"name\" is required for generic action buttons");
    }
    if (type.equals("reset") && getAttributes().containsKey(NAME_ATTR)) {
      throw new InvalidInputException("Attribute \"name\" is allowed for generic action buttons only");
    }
    
    assertContentModel(Collections.singleton(TextNode.class));
    assertContainsChildOfType(Collections.singleton(TextNode.class));
  }
}
