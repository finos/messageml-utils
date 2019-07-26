package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormNode;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Class representing a Symphony Elements form
 *
 * @author lumoura
 * @since 03/21/19
 */
public class Form extends Element {
  public static final String MESSAGEML_TAG = "form";
  private static final String ID_ATTR = "id";

  private static List<String> builtFormIds = new ArrayList<>();
  
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

    assertUniqueFormId();
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ID_ATTR:
        setAttribute(ID_ATTR, getStringAttribute(item));
        builtFormIds.add(getAttribute(ID_ATTR));
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");  
    }
  }

  /**
   * Assert that the form id is unique.
   *
   * @throws InvalidInputException
   */
  private void assertUniqueFormId() throws InvalidInputException {
    if (Collections.frequency(builtFormIds, getAttribute(ID_ATTR)) > 1) {
      throw new InvalidInputException("MessageML cannot contain multiple forms using the same id");
    }
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new FormNode();
  }
}