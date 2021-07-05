package org.symphonyoss.symphony.messageml.elements;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.ButtonNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Node;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import static org.symphonyoss.symphony.messageml.elements.FormElement.NAME_ATTR;
import static org.symphonyoss.symphony.messageml.elements.FormElement.TYPE_ATTR;

/**
 * This class specify the Symphony Element Button represented by tag name "button". A Button can be included either
 * inside Forms or in UIActions (at least one is required).
 * Depending on the location, the messageML representation can be different. When is part of a Form it can contain the
 * following attributes:
 * <ul>
 *    <li>name (required) -> used to identify the button</li>
 *    <li>type -> default "action", specify the type of the button. Allowed values are "action", "reset" and "cancel"</li>
 *    <li>class -> can be "primary", "secondary", "tertiary" (deprecated: "primary-destructive","secondary-destructive")</li>
 *    <li>title -> description displayed as a hint<l/i>
 * <ul/>
 * When the Button is included inside a UIAction, only "class" and "title" are allowed as attributes.
 */
public class Button extends Element {

  Logger logger = LoggerFactory.getLogger(Button.class);

  public static final String MESSAGEML_TAG = "button";
  public static final String ACTION_TYPE = "action";
  public static final String CANCEL_TYPE = "cancel";
  public static final String RESET_TYPE = "reset";

  private static final Set<String> VALID_CLASSES = new HashSet<>(Arrays.asList("primary", "secondary", "tertiary", "destructive",
      "primary-destructive", "secondary-destructive")); // primary-destructive, secondary-destructive are deprecated
  private static final Set<String> VALID_TYPES = new HashSet<>(Arrays.asList(ACTION_TYPE, RESET_TYPE, CANCEL_TYPE));

  public Button(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
    if (!isUIActionButton()) {
      setAttribute(TYPE_ATTR, ACTION_TYPE);
    }
  }

  @Override
  public void buildAttribute(MessageMLParser parser, Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case TYPE_ATTR:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      // The button can a have tooltips but is not a tooltipable element because it dont generate the span with tooltip
      case CLASS_ATTR:
        if (getStringAttribute(item).contains("-destructive")) {
          logger.info("Button class cannot be a destructive one, replacing it accordingly.");
        }
        setAttribute(item.getNodeName(), StringUtils.removeEnd(getStringAttribute(item), "-destructive"));
        break;
      case TooltipableElement.TITLE:
        if (format != FormatEnum.MESSAGEML) {
          throwInvalidInputException(item);
        }
        setAttribute(TooltipableElement.TITLE, getStringAttribute(item));
        break;
      case TooltipableElement.DATA_TITLE:
        if (format != FormatEnum.PRESENTATIONML) {
          throwInvalidInputException(item);
        }
        setAttribute(TooltipableElement.DATA_TITLE, getStringAttribute(item));
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  void asPresentationML(XmlPrintStream out, MessageMLContext context) {
    out.openElement(getPresentationMLTag(), getPresentationMLAttributes());
    for (Element child : getChildren()) {
      child.asPresentationML(out, context);
    }
    out.closeElement();
  }

  private Map<String, String> getPresentationMLAttributes() {
    Map<String, String> attributes = getAttributes();
    if (format == FormatEnum.MESSAGEML && attributes.containsKey(TooltipableElement.TITLE)) {
      Map<String, String> presentationAttributes = new LinkedHashMap<>(attributes);
      presentationAttributes.put(TooltipableElement.DATA_TITLE, attributes.get(TooltipableElement.TITLE));
      presentationAttributes.remove(TooltipableElement.TITLE);
      return presentationAttributes;
    }
    return attributes;
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new ButtonNode();
  }

  @Override
  public void validate() throws InvalidInputException {
    assertParentAtAnyLevel(Arrays.asList(Form.class, UIAction.class));
    validateCommonAttributes();
    if (isUIActionButton()) {
      validateUIActionButton();
    } else {
      validateFormButton();
    }
  }

  private void validateCommonAttributes() throws InvalidInputException {
    String clazz = getAttribute(CLASS_ATTR);
    if (clazz != null && !VALID_CLASSES.contains(clazz)) {
      throw new InvalidInputException("Attribute \"class\" must be \"primary\", \"secondary\", " +
              "\"tertiary\" or \"destructive\" (\"primary-destructive\" and \"secondary-destructive\" are deprecated)");
    }
  }

  private void validateUIActionButton() throws InvalidInputException {
    if (getAttribute(TYPE_ATTR) != null || getAttribute(NAME_ATTR) != null) {
      throw new InvalidInputException("Attributes \"type\" and \"name\" are not allowed on a button inside a UIAction.");
    }
  }

  private void validateFormButton() throws InvalidInputException {
    String type = getAttribute(TYPE_ATTR);
    String name = getAttribute(NAME_ATTR);

    if (!VALID_TYPES.contains(type)) {
      throw new InvalidInputException("Attribute \"type\" must be \"action\", \"reset\" or \"cancel\"");
    }

    if (type.equals(ACTION_TYPE) && StringUtils.isBlank(name)) {
      throw new InvalidInputException("Attribute \"name\" is required for action buttons");
    }

    if (type.equals(CANCEL_TYPE) && StringUtils.isBlank(name)) {
      throw new InvalidInputException("Attribute \"name\" is required for cancel buttons");
    }

    if (type.equals(RESET_TYPE) && getAttributes().containsKey(NAME_ATTR)) {
      throw new InvalidInputException("Attribute \"name\" is allowed for action buttons only");
    }

    assertContentModel(Collections.singleton(TextNode.class));
    assertContainsChildOfType(Collections.singleton(TextNode.class));
  }

  private boolean isUIActionButton() {
    return this.getParent().getClass().equals(UIAction.class);
  }

  @Override
  public void updateBiContext(BiContext context) {
    Map<String, Object> attributesMapBi = new HashMap<>();

    this.putStringIfPresent(attributesMapBi, BiFields.STYLE_COLOR.getValue(), CLASS_ATTR);
    this.putStringIfPresent(attributesMapBi, BiFields.TYPE.getValue(), TYPE_ATTR);

    context.addItem(new BiItem(BiFields.BUTTON.getValue(), attributesMapBi));
  }
}
