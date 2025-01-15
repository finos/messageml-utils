package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.TextFieldNode;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;


/**
 * Class representing a Text Field inside a Form.
 * @author Lucas Macedo
 * @since 06/07/2019
 */
public class TextField extends FormElement implements RegexElement, LabelableElement, TooltipableElement, MinMaxLengthElement {

  public static final String MESSAGEML_TAG = "text-field";
  public static final String ELEMENT_ID = "textfield";
  public static final String PRESENTATIONML_INPUT_TYPE = "text";

  private static final String REQUIRED_ATTR = "required";
  private static final String MASKED_ATTR = "masked";
  private static final String PLACEHOLDER_ATTR = "placeholder";
  private static final String VALUE_ATTR = "value";
  private static final String DISABLED_ATTR = "disabled";
  private static final String READONLY_ATTR = "readonly";
  private static final String PRESENTATIONML_MASKED_ATTR = "data-masked";
  private static final String MML_AUTO_SUBMIT_ATTR = "auto-submit";
  private static final String AUTO_SUBMIT_ATTR = "data-auto-submit";

  private static final Set<String> VALID_BOOLEAN_VALUES = new HashSet<>(Arrays.asList("true", "false"));
  private static final Integer MIN_ALLOWED_LENGTH = 1;
  private static final Integer MAX_ALLOWED_LENGTH = 128;

  private final static String MARKDOWN = "Text Field";

  public TextField(Element parent, FormatEnum messageFormat) {
    super(parent, MESSAGEML_TAG, messageFormat);
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    if (getAttribute(NAME_ATTR) == null) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }

    if (getAttribute(REQUIRED_ATTR) != null) {
      assertAttributeValue(REQUIRED_ATTR, VALID_BOOLEAN_VALUES);
    }

    if (getAttribute(MASKED_ATTR) != null) {
      assertAttributeValue(MASKED_ATTR, VALID_BOOLEAN_VALUES);
    }

    if (getAttribute(DISABLED_ATTR) != null) {
      assertAttributeValue(DISABLED_ATTR, VALID_BOOLEAN_VALUES);
    }

    if (getAttribute(READONLY_ATTR) != null) {
      assertAttributeValue(READONLY_ATTR, VALID_BOOLEAN_VALUES);
    }

    if (getAttribute(AUTO_SUBMIT_ATTR) != null) {
      assertAttributeValue(AUTO_SUBMIT_ATTR, VALID_BOOLEAN_VALUES);
    }

    assertAttributeNotBlank(NAME_ATTR);
    assertContentModel(Collections.singleton(TextNode.class));
    validateMinAndMaxLengths();
  }

  @Override
  public void buildAll(MessageMLParser parser, org.w3c.dom.Element element)
      throws InvalidInputException, ProcessingException {
    switch (getFormat()) {
      case MESSAGEML:
        super.buildAll(parser, element);
        break;
      case PRESENTATIONML:
        this.buildAllFromPresentationML(parser, element);
        break;
    }
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new TextFieldNode(getAttribute(PLACEHOLDER_ATTR), hasExactNumberOfChildren(1) ? getChild(0).asText() : null,
        getAttribute(LABEL), getAttribute(TITLE));
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case REQUIRED_ATTR:
      case PLACEHOLDER_ATTR:
      case MINLENGTH_ATTR:
      case MAXLENGTH_ATTR:
      case MASKED_ATTR:
      case PATTERN_ATTR:
      case PATTERN_ERROR_MESSAGE_ATTR:
      case LABEL:
      case TITLE:
      case DISABLED_ATTR:
      case READONLY_ATTR:
      case FORMNOVALIDATE_ATTR:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case ID_ATTR:
        if(format != FormatEnum.PRESENTATIONML){
          throwInvalidInputException(item);
        }
        fillAttributes(parser, item);
        break;
      case MML_AUTO_SUBMIT_ATTR:
        setAttribute(AUTO_SUBMIT_ATTR, getStringAttribute(item));
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  private void buildAllFromPresentationML(MessageMLParser parser, org.w3c.dom.Element element)
      throws InvalidInputException {
    NamedNodeMap attr = element.getAttributes();
    NodeList children = element.getChildNodes();

    if (children != null && children.getLength() > 0) {
      throw new InvalidInputException(
          "Element \"" + this.getMessageMLTag() + "\" may not have child elements or text content");
    }

    for (int i = 0; i < attr.getLength(); i++) {

      buildAttributeFromPresentationML(parser, attr.item(i));
    }
  }

  private void buildAttributeFromPresentationML(MessageMLParser parser, org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case REQUIRED_ATTR:
      case PLACEHOLDER_ATTR:
      case MINLENGTH_ATTR:
      case MAXLENGTH_ATTR:
      case LABEL:
      case PATTERN_ATTR:
      case PRESENTATIONML_PATTERN_ERROR_MESSAGE_ATTR:
      case FORMNOVALIDATE_PML_ATTR:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case VALUE_ATTR:
        addChild(new TextNode(this, getStringAttribute(item)));
        break;
      case PRESENTATIONML_MASKED_ATTR:
        setAttribute(MASKED_ATTR, getStringAttribute(item));
        break;
      case ID_ATTR:
        fillAttributes(parser, item);
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public Map<String, String> getOtherAttributes() {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();

    presentationAttrs.put(TYPE_ATTR, PRESENTATIONML_INPUT_TYPE);
    presentationAttrs.put(NAME_ATTR, getAttribute(NAME_ATTR));

    if (getAttribute(PLACEHOLDER_ATTR) != null) {
      presentationAttrs.put(PLACEHOLDER_ATTR, getAttribute(PLACEHOLDER_ATTR));
    }

    if (getAttribute(REQUIRED_ATTR) != null) {
      presentationAttrs.put(REQUIRED_ATTR, getAttribute(REQUIRED_ATTR));
    }

    if (getAttribute(MASKED_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_MASKED_ATTR, getAttribute(MASKED_ATTR));
    }

    if (getAttribute(MINLENGTH_ATTR) != null) {
      presentationAttrs.put(MINLENGTH_ATTR, getAttribute(MINLENGTH_ATTR));
    }

    if (getAttribute(MAXLENGTH_ATTR) != null) {
      presentationAttrs.put(MAXLENGTH_ATTR, getAttribute(MAXLENGTH_ATTR));
    }

    if (getAttribute(DISABLED_ATTR) != null) {
      presentationAttrs.put(DISABLED_ATTR, getAttribute(DISABLED_ATTR));
    }

    if (getAttribute(READONLY_ATTR) != null) {
      presentationAttrs.put(READONLY_ATTR, getAttribute(READONLY_ATTR));
    }
    if (getAttribute(AUTO_SUBMIT_ATTR) != null) {
      presentationAttrs.put(AUTO_SUBMIT_ATTR, getAttribute(AUTO_SUBMIT_ATTR));
    }

    if(getAttribute(FORMNOVALIDATE_ATTR) != null) {
      presentationAttrs.put(FORMNOVALIDATE_PML_ATTR, getAttribute(FORMNOVALIDATE_ATTR));
    }

    if(getAttribute(FORMNOVALIDATE_PML_ATTR) != null) {
      presentationAttrs.put(FORMNOVALIDATE_PML_ATTR, getAttribute(FORMNOVALIDATE_PML_ATTR));
    }

    if (getChildren() != null && getChildren().size() == 1) {
      presentationAttrs.put(VALUE_ATTR, getChildren().get(0).asText());
    }

    return presentationAttrs;
  }

  @Override
  public void updateBiContext(BiContext context) {
    Map<String, Object> attributesMapBi = new HashMap<>();

    this.putTypeIfPresent(attributesMapBi);
    this.putOneIfPresent(attributesMapBi, BiFields.TITLE.getValue(), TITLE);
    this.putOneIfPresent(attributesMapBi, BiFields.PLACEHOLDER.getValue(), PLACEHOLDER_ATTR);
    this.putOneIfPresent(attributesMapBi, BiFields.LABEL.getValue(), LABEL);
    this.putOneIfPresent(attributesMapBi, BiFields.REQUIRED.getValue(), REQUIRED_ATTR);
    this.computeAndPutValidationProperties(attributesMapBi);

    if (this.hasElementInitialValue()) {
      attributesMapBi.put(BiFields.DEFAULT.getValue(), 1);
    }

    context.addItem(new BiItem(BiFields.TEXT_FIELD.getValue(), attributesMapBi));
  }

  @Override
  public String getPresentationMLTag() {
    return INPUT_TAG;
  }

  @Override
  public boolean areNestedElementsAllowed(){
    return false;
  }

  @Override
  public String getElementId(){
    return ELEMENT_ID;
  }

  @Override
  public String getElementType() {
        return MESSAGEML_TAG;
    }

  @Override
  public boolean hasElementInitialValue() {
    return getChildren() != null && getChildren().size() == 1 && getChild(0) instanceof TextNode;
  }

  @Override
  public String getElementInitialValue() {
    return ((TextNode) getChild(0)).getText();
  }

  @Override
  public String getAttributeValue(String attributeName) {
    return getAttribute(attributeName);
  }

  @Override
  public Integer getMinValueAllowed() {
    return MIN_ALLOWED_LENGTH;
  }

  @Override
  public Integer getMaxValueAllowed() {
    return MAX_ALLOWED_LENGTH;
  }

  private void putTypeIfPresent(Map<String, Object> attributesMap) {
    String value = getAttributes().get(MASKED_ATTR);

    if (value != null && Boolean.TRUE.equals(Boolean.valueOf(value))) {
      attributesMap.put(BiFields.TYPE.getValue(), BiFields.TYPE_MASKED_TRUE.getValue());
    } else if (value != null && Boolean.FALSE.equals(Boolean.valueOf(value))) {
      attributesMap.put(BiFields.TYPE.getValue(), BiFields.TYPE_MASKED_FALSE.getValue());
    }
  }

  private void computeAndPutValidationProperties(Map<String, Object> attributesMapBi) {
    boolean validationMin = getAttribute(MINLENGTH_ATTR) != null;
    boolean validationMax = getAttribute(MAXLENGTH_ATTR) != null;
    boolean validationPattern = getAttribute(PATTERN_ATTR) != null;
    boolean hasValidation = validationMin || validationMax || validationPattern;

    if (validationMin) {
      attributesMapBi.put(BiFields.VALIDATION_MIN.getValue(), 1);
    }

    if (validationMax) {
      attributesMapBi.put(BiFields.VALIDATION_MAX.getValue(), 1);
    }

    if (validationPattern) {
      attributesMapBi.put(BiFields.VALIDATION_PATTERN.getValue(), 1);
    }

    if (hasValidation) {
      attributesMapBi.put(BiFields.VALIDATION.getValue(), 1);
    }
  }
}
