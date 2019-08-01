package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormNode;

import java.util.Arrays;
import java.util.Collection;
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
    assertChildrenNotExceedingMaxCount(Arrays.asList(Checkbox.class, Radio.class));

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

  /**
   * Assert that children with any of the informed types do not exceed the maximum count allowed.
   *
   * @param elementTypes
   * @throws InvalidInputException
   */
  private void assertChildrenNotExceedingMaxCount(Collection<Class<? extends Element>> elementTypes) throws InvalidInputException {
    boolean hasExceeded = elementTypes.stream().anyMatch(type -> findElements(type).size() > MAX_COUNT_PER_CHILD_TYPE);
    if (hasExceeded) {
      throw new InvalidInputException(String.format("Element \"form\" cannot have more than %s children of the following elements: [%s].",
          MAX_COUNT_PER_CHILD_TYPE, getElementsNameByClassName(elementTypes)));
    }
  }
}
