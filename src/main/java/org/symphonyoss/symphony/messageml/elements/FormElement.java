package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collection;
import java.util.Collections;

public class FormElement extends Element {
  FormElement(Element parent) {
    super(parent);
  }

  FormElement(Element parent, String messageMLTag) {
    super(parent, messageMLTag);
  }

  FormElement(Element parent, String messageMLTag, FormatEnum format) {
    super(parent, messageMLTag, format);
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
}
