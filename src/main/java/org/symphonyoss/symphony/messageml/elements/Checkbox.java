package org.symphonyoss.symphony.messageml.elements;

import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.CheckboxNode;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * Class representing a Checkbox inside a Form.
 * @author Cristiano Faustino
 * @since 05/29/2019
 */
public class Checkbox extends GroupedElement implements LabelableElement {
  public static final String MESSAGEML_TAG = "checkbox";
  public static final String PRESENTATIONML_INPUT_TYPE = "checkbox";
  public static final String PRESENTATIONML_DIV_CLASS = "checkbox-group";

  private static final String MARKDOWN = "Checkbox";

  public Checkbox(Element parent, FormatEnum messageFormat) {
    super(parent, MESSAGEML_TAG, messageFormat);
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    if (getAttribute(NAME_ATTR) == null) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }

    if (getAttribute(CHECKED_ATTR) != null) {
      assertAttributeValue(CHECKED_ATTR, Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    if (!getChildren().isEmpty()) {
      assertContentModel(Arrays.asList(TextNode.class, Bold.class, Italic.class));
    }

    assertAttributeNotBlank(NAME_ATTR);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case VALUE_ATTR:
      case CHECKED_ATTR:
      case LABEL:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case ID_ATTR:
        if(this.format != FormatEnum.PRESENTATIONML){
          throwInvalidInputException(item);
        }
        fillAttributes(parser, item);
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public Node asMarkdown() {
    if (hasExactNumberOfChildren(1)) {
      return new CheckboxNode(getChildren().get(0).asText());
    }
    else {
      return new CheckboxNode();
    }
  }

  @Override
  public void updateBiContext(BiContext context) {
    Map<String, Object> attributesMapBi = new HashMap<>();

    attributesMapBi.put(BiFields.OPTIONS_COUNT.getFieldName(), 1);
    this.putOneIfPresent(attributesMapBi, BiFields.LABEL.getFieldName(), LABEL);
    this.computeAndPutDefault(context, attributesMapBi);

    context.updateItem(BiFields.CHECKBOX.getFieldName(), attributesMapBi);
  }

  /**
   * This method will compute default property for this element : if {@link
   * GroupedElement#CHECKED_ATTR attribute is set to true}
   * It will update the context if and only if this current option is the first option
   * of the checkboxes group to have default value set to true
   *
   * If {@link GroupedElement#CHECKED_ATTR} attribute is not explicitly set to true,
   * this default property will be considered as not set like in following :
   * <pre><checkbox checked=\"false\">Check Me if you can!</checkbox></pre>
   * <pre><checkbox checked=\"somethingElse\">Check Me if you can!</checkbox></pre>
   */
  private void computeAndPutDefault(BiContext context, Map<String, Object> attributesMapBi) {
    String isChecked = getAttribute(CHECKED_ATTR);
    boolean isDefaultAlreadySet =
        context.isAttributeSet(BiFields.CHECKBOX.getFieldName(), BiFields.DEFAULT.getFieldName());

    if (isChecked != null && Boolean.TRUE.equals(Boolean.valueOf(isChecked)) && !isDefaultAlreadySet) {
      attributesMapBi.put(BiFields.DEFAULT.getFieldName(), 1);
    }
  }

  @Override
  protected String getPresentationMLInputType() {
    return PRESENTATIONML_INPUT_TYPE;
  }

  @Override
  protected String getPresentationMLDivClass() {
    return PRESENTATIONML_DIV_CLASS;
  }

  @Override
  public String getElementId() {
    return MESSAGEML_TAG;
  }
}
