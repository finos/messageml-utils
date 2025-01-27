package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class FormElement extends Element {
  public static final String INPUT_TAG = "input";
  public static final String TYPE_ATTR = "type";
  public static final String FORMNOVALIDATE_ATTR = "formnovalidate";
  public static final String FORMNOVALIDATE_PML_ATTR = "data-formnovalidate";


  protected static final String NAME_ATTR = "name";

  public FormElement(Element parent, String messageMLTag) {
    super(parent, messageMLTag);
  }

  public FormElement(Element parent, String messageMLTag, FormatEnum format) {
    super(parent, messageMLTag, format);
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();
    if (getAttribute(FORMNOVALIDATE_ATTR) != null) {
      assertAttributeIsBoolean(FORMNOVALIDATE_ATTR);
    }
    if (getAttribute(FORMNOVALIDATE_PML_ATTR) != null) {
      assertAttributeIsBoolean(FORMNOVALIDATE_PML_ATTR);
    }
    assertParentAtAnyLevel(Collections.singleton(Form.class));
  }
}
