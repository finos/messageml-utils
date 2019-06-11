package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

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
}
