package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collection;
import java.util.Collections;

public class FormElement extends Element {
  protected static final String NAME_ATTR = "name";

  FormElement(Element parent, String messageMLTag) {
    super(parent, messageMLTag);
  }

  @Override
  public void validate() throws InvalidInputException {
    assertParent(Collections.singleton(Form.class));
  }

  @Override
  public void assertParent(Collection<Class<? extends Element>> permittedParents) throws InvalidInputException {
    if (!permittedParents.contains(this.getParent().getClass())) {
      String permittedParentsClassAsString = permittedParents.stream()
          .map(permittedParentClass -> permittedParentClass.getSimpleName().toLowerCase())
          .reduce((item, anotherItem) -> String.format("%s, %s", item, anotherItem))
          .orElse("");
      throw new InvalidInputException(String.format("Element \"%s\" can only be a child of the following elements: \"%s\"",
          this.getMessageMLTag(), permittedParentsClassAsString));
    }
  }

  protected void assertContainsElement(Class<? extends Element> childElement) throws InvalidInputException {
    boolean hasElementAsChild = this.getChildren()
        .stream()
        .anyMatch(child -> child.getClass().equals(childElement));

    if (!hasElementAsChild) {
      throw new InvalidInputException(String.format("The \"%s\" element must have at least one \"%s\" as its child.",
          getMessageMLTag(), getElementName(childElement)));
    }
  }

  private String getElementName(Class<? extends Element> element) {
    return element.getClass().equals(TextNode.class) ? "text" : element.getSimpleName().toLowerCase();
  }
}
