package org.symphonyoss.symphony.messageml.elements;

import static org.apache.commons.lang3.StringUtils.containsWhitespace;
import static org.apache.commons.lang3.StringUtils.isEmpty;

import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.w3c.dom.Node;

import java.util.Arrays;
import java.util.List;

public class Dialog extends Element {

  public static final String MESSAGEML_TAG = "dialog";

  public static final String WIDTH_ATTR = "width";
  public static final String MEDIUM_WIDTH = "medium";
  public static final List<String> ALLOWED_WIDTH_VALUES = Arrays.asList("small", MEDIUM_WIDTH, "large", "full-width");

  public Dialog(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
  }

  @Override
  public Boolean hasIdAttribute() {
    return true;
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    String id = getAttribute(ID_ATTR);
    if (isEmpty(id) || containsWhitespace(id)) {
      throw new InvalidInputException("id attribute is mandatory and must not contain any whitespace");
    }

    String width = getAttribute(WIDTH_ATTR);
    if (width == null) {
      setAttribute(WIDTH_ATTR, MEDIUM_WIDTH);
    } else if (!ALLOWED_WIDTH_VALUES.contains(width)) {
      throw new InvalidInputException("width attribute value should be among " + ALLOWED_WIDTH_VALUES);
    }
  }

  @Override
  protected void buildAttribute(MessageMLParser parser, Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ID_ATTR:
      case WIDTH_ATTR:
        setAttribute(item.getNodeName(), item.getNodeValue());
    }
  }
//
//  @Override
//  void updateBiContext(BiContext context) {
//    context.addItem(new BiItem(BiFields.DIALOG.getValue(), new HashMap<>()));
//  }
}
