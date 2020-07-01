package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * Interface to add label to elements
 *
 * Simply implement it into the element when you want to add label support
 *
 * Beware:
 *  If the element implementing this interface overrides {@link Element#buildAttribute(org.symphonyoss.symphony.messageml.MessageMLParser, org.w3c.dom.Node)} and/or {@link Element#asPresentationML(XmlPrintStream out,
 *       MessageMLContext context)} without calling super, it is needed to manage manually LABEL attribute and {@link #splittableAsPresentationML(XmlPrintStream out,
 *       MessageMLContext context)} method
 *
 * @author enrico.molino (10/06/2020)
 *
 */
public interface LabelableElement extends SplittableElement {

  String LABEL = "label";
  String LABEL_FOR = "for";
  int LABEL_MAX_LENGTH = 256;
  String LABEL_TOO_LONG_ERR = "The attribute label value is too long. Max length is: %d";

  /**
   *
   * @return label tag. Normally you dont need to override the default method
   */
  default String getPresentationMLLabelTag(){
    return LABEL;
  }

  /**
   * Used internally, normally you dont need to override the default method
   *
   * @param id used to generate the unique element id
   * @return
   */
  default Map<String, String> getLabelAttribute(String id){
    Map<String, String> labelAttributes = new HashMap<>();
    labelAttributes.put(LABEL_FOR, id);
    return labelAttributes;
  }

  /**
   * @return true if label exists
   * Normally, you dont need to override the default method
   */
  default boolean isLabel(){
    return getAttribute(LABEL) != null;
  }

  /**
   *
   * Check if the PresentationML node is a label
   */
  default boolean isLabelNode(org.w3c.dom.Node item){
    return getPresentationMLLabelTag().equals(item.getNodeName());
  }

  /**
   *
   * Validate label max length, if present
   */
  default void validateLabel() throws InvalidInputException {
    String labelAttribute = getAttribute(LABEL);
    if(labelAttribute != null && labelAttribute.length() > LABEL_MAX_LENGTH){
      throw new InvalidInputException(String.format(LABEL_TOO_LONG_ERR, LABEL_MAX_LENGTH));
    }
  }
}
