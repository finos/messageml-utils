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
  public static final String ACTION_TYPE = "action";
  public static final String RESET_TYPE = "reset";
  public static final String PRIMARY_CLASS = "primary";
  public static final String SECONDARY_CLASS = "secondary";
  public static final String PRIMARY_DESTR_CLASS = "primary-destructive";
  public static final String SECONDARY_DESTR_CLASS = "secondary-destructive";

  private static final Set<String> VALID_CLASSES = new HashSet<>(Arrays.asList(PRIMARY_CLASS, SECONDARY_CLASS,
      PRIMARY_DESTR_CLASS, SECONDARY_DESTR_CLASS));
  private static final Set<String> VALID_TYPES = new HashSet<>(Arrays.asList(ACTION_TYPE, RESET_TYPE));

  public Button(Element parent) {
    super(parent, MESSAGEML_TAG);
    setAttribute(TYPE_ATTR, ACTION_TYPE);
  }

  @Override
  public void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
        setAttribute(NAME_ATTR, getStringAttribute(item));
        break;

      case TYPE_ATTR:
        setAttribute(TYPE_ATTR, getStringAttribute(item));
        break;

      case CLASS_ATTR:
        setAttribute(CLASS_ATTR, getStringAttribute(item));
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
    if (type.equals(ACTION_TYPE) && StringUtils.isBlank(name)) {
      throw new InvalidInputException("Attribute \"name\" is required for action buttons");
    }
    if (type.equals(RESET_TYPE) && getAttributes().containsKey(NAME_ATTR)) {
      throw new InvalidInputException("Attribute \"name\" is allowed for action buttons only");
    }

    assertContentModel(Collections.singleton(TextNode.class));
    assertContainsChildOfType(Collections.singleton(TextNode.class));
  }
}
