package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class TextField extends FormElement {
  
  public static final String MESSAGEML_TAG = "text-field";
  private static final String NAME_ATTR = "name";
  private static final String REQUIRED_ATTR = "required";
  private static final Set<String> VALID_VALUES_FOR_REQUIRED_ATTR = new HashSet<>(Arrays.asList("true", "false"));

  public TextField(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    if (getAttribute(NAME_ATTR) == null) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }

    validateRequiredAttribute(getAttribute(REQUIRED_ATTR));
    
    
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    
  }


  private void validateRequiredAttribute(String requiredAttrValue) throws InvalidInputException {
    if (requiredAttrValue != null && !VALID_VALUES_FOR_REQUIRED_ATTR.contains(requiredAttrValue)) {
      throw new InvalidInputException(String.format("Attribute \"%s\" must have one of the following values: %s",
          REQUIRED_ATTR, VALID_VALUES_FOR_REQUIRED_ATTR));
    }
  }
  
  
}
