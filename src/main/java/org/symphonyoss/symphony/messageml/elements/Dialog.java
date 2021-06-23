package org.symphonyoss.symphony.messageml.elements;

import static org.apache.commons.lang3.StringUtils.containsWhitespace;
import static org.apache.commons.lang3.StringUtils.isEmpty;

import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Node;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Dialog extends Element {

  public static final String MESSAGEML_TAG = "dialog";

  public static final String STATE_ATTR = "state";
  public static final String FALSE_STATE = "false";
  public static final List<String> ALLOWED_STATE_VALUES = Arrays.asList(FALSE_STATE, "open", "close");

  public static final String WIDTH_ATTR = "width";
  public static final String MEDIUM_WIDTH = "medium";
  public static final List<String> ALLOWED_WIDTH_VALUES = Arrays.asList("small", MEDIUM_WIDTH, "large", "full-width");

  private static final String DATA_ATTRIBUTE_PREFIX = "data-";

  public Dialog(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
  }

  @Override
  public Boolean hasIdAttribute() {
    return true;
  }

  @Override
  protected void buildAttribute(MessageMLParser parser, Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ID_ATTR:
      case WIDTH_ATTR:
      case STATE_ATTR:
        setAttribute(item.getNodeName(), item.getNodeValue());
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    checkAttributes();
    validateChildrenTypes();
  }

  @Override
  public void asPresentationML(XmlPrintStream out, MessageMLContext context) {
    out.openElement(getPresentationMLTag(), getPresentationMLAttributes());
    for (Element child : getChildren()) {
      child.asPresentationML(out, context);
    }
    out.closeElement();
  }

  private Map<String, String> getPresentationMLAttributes() {
    Map<String, String> pmlAttributes = new HashMap<>();

    for (Map.Entry<String, String> mmlAttribute : getAttributes().entrySet()) {
      if (mmlAttribute.getKey().equals(ID_ATTR)) {
        pmlAttributes.put(mmlAttribute.getKey(), mmlAttribute.getValue());
      } else {
        pmlAttributes.put(DATA_ATTRIBUTE_PREFIX + mmlAttribute.getKey(), mmlAttribute.getValue());
      }
    }
    return pmlAttributes;
  }

  private void checkAttributes() throws InvalidInputException {
    validateIdAttribute();
    checkAttributeOrPutDefaultValue(WIDTH_ATTR, MEDIUM_WIDTH, ALLOWED_WIDTH_VALUES);
    checkAttributeOrPutDefaultValue(STATE_ATTR, FALSE_STATE, ALLOWED_STATE_VALUES);
  }

  private void validateIdAttribute() throws InvalidInputException {
    String id = getAttribute(ID_ATTR);
    if (isEmpty(id) || containsWhitespace(id)) {
      throw new InvalidInputException("The attribute \"id\" is required and must not contain any whitespace");
    }
  }

  private void checkAttributeOrPutDefaultValue(String attributeName, String defaultValue, List<String> allowedValues)
      throws InvalidInputException {
    if (getAttribute(attributeName) == null) {
      setAttribute(attributeName, defaultValue);
    }
    assertAttributeValue(attributeName, allowedValues);
  }

  private void validateChildrenTypes() throws InvalidInputException {
    assertContainsAlwaysChildOfType(Collections.singleton(DialogChild.Title.class));
    assertContainsAlwaysChildOfType(Collections.singleton(DialogChild.Body.class));
    validateNoOtherChildrenTypes();
  }

  private void validateNoOtherChildrenTypes() throws InvalidInputException {
    final boolean areAllChildrenOfAllowedTypes =
        getChildren().stream().allMatch(element -> element instanceof DialogChild);
    if (!areAllChildrenOfAllowedTypes) {
      throw new InvalidInputException("A dialog can only contain tags \"title\", \"body\", \"footer\"");
    }
  }
}
