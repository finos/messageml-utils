package org.symphonyoss.symphony.messageml.elements;

import static java.lang.String.format;
import static org.symphonyoss.symphony.messageml.elements.Button.ACTION_TYPE;
import static org.symphonyoss.symphony.messageml.elements.FormElement.TYPE_ATTR;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormNode;

import java.util.Arrays;
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
  private static final int MAX_COUNT_PER_CHILD_TYPE = 20;

  private static final String ERR_MSG_MISSING_ACTION_BTN = "The form with id '%s' should have at least one action button";

  public Form(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public Boolean hasIdAttribute() {
    return true;
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();
    assertNotParentAtAnyLevel(Collections.singletonList(this.getClass()));
    assertChildrenNotExceedingMaxCount(Arrays.asList(Checkbox.class, Radio.class), MAX_COUNT_PER_CHILD_TYPE);

    if (getAttribute(ID_ATTR) == null) {
      throw new InvalidInputException("The attribute \"id\" is required");
    }
    assertAttributeNotBlank(ID_ATTR);
    assertAtLeastOneActionButton();
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

  private void assertAtLeastOneActionButton() throws InvalidInputException {
    boolean hasActionButton = findElements(Button.class).stream()
        .anyMatch(element -> ACTION_TYPE.equals(element.getAttribute(TYPE_ATTR)));

    if (!hasActionButton) {
      throw new InvalidInputException(format(ERR_MSG_MISSING_ACTION_BTN, getAttribute(ID_ATTR)));
    }
  }

}
