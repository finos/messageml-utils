package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.NamedNodeMap;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * Interface to add tooltip to elements
 *
 * Simply implement it into the element when you want to add tooltip support
 *
 * Beware:
 *  If the element implementing this interface overrides {@link Element#buildAttribute(org.symphonyoss.symphony.messageml.MessageMLParser, org.w3c.dom.Node)}
 *  and/or {@link Element#asPresentationML(XmlPrintStream out, MessageMLContext context)} without calling super,
 *  it is needed to manage manually TITLE attribute and {@link SplittableElement#splittableAsPresentationML(XmlPrintStream out, MessageMLContext context)} method
 *
 * @author enrico.molino (24/06/2020)
 *
 */
public interface TooltipableElement extends SplittableElement {

  String TITLE = "title";
  String DATA_TITLE = "data-title";
  String DATA_TARGET_ID = "data-target-id";
  String TOOLTIP_CLASS = "info-hint";
  int TOOLTIP_MAX_LENGTH = 256;
  String TOOLTIP_TOO_LONG_ERR = "The attribute title value is too long. Max length is: %d";
  String TOOLTIPABLE_PRESENTATIONML = Span.MESSAGEML_TAG;

  /**
   *
   * @return label tag. Normally you dont need to override the default method
   */
  default String getPresentationMLTooltipTag(){
    return TOOLTIPABLE_PRESENTATIONML;
  }

  /**
   * Used internally, normally you dont need to override the default method
   *
   * @param id used to generate the unique element id
   * @return
   */
  default Map<String, String> getTooltipAttributes(String id){
    Map<String, String> spanAttributes = new LinkedHashMap<>();
    spanAttributes.put(Span.CLASS_ATTR, TOOLTIP_CLASS);
    spanAttributes.put(DATA_TARGET_ID, id);
    spanAttributes.put(DATA_TITLE, getAttribute(TITLE));
    return spanAttributes;
  }

  /**
   * @return true if tooltip exists
   * Normally, you dont need to override the default method
   */
  default boolean isTooltip(){
    return getAttribute(TITLE) != null;
  }

  /**
   *
   * Check if the PresentationML node is a tooltip
   */
  static boolean isTooltipNode(org.w3c.dom.Node item){
    if(!TOOLTIPABLE_PRESENTATIONML.equals(item.getNodeName())){
      return false;
    }
    NamedNodeMap attributes = item.getAttributes();
    if(attributes.getNamedItem(DATA_TARGET_ID) == null
        || attributes.getNamedItem(DATA_TITLE) == null
        || attributes.getNamedItem(Span.CLASS_ATTR) == null
        || !TOOLTIP_CLASS.equals(attributes.getNamedItem(Span.CLASS_ATTR).getNodeValue())){
      return false;
    }
    return true;
  }

  /**
   *
   * Validate tooltip max length, if present
   */
  default void validateTooltip() throws InvalidInputException {
    String tooltipAttribute = getAttribute(TITLE);
    if(tooltipAttribute != null && tooltipAttribute.length() > TOOLTIP_MAX_LENGTH){
      throw new InvalidInputException(String.format(TOOLTIP_TOO_LONG_ERR, TOOLTIP_MAX_LENGTH));
    }
  }
}
