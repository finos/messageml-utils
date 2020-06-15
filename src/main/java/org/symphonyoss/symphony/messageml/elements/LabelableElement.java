package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.util.Collections;
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
 *       MessageMLContext context)} without calling super, it is needed to manage manually LABEL attribute and {@link #labelAsPresentationML(XmlPrintStream out,
 *       MessageMLContext context)} method
 *
 * @author enrico.molino (10/06/2020)
 *
 */
public interface LabelableElement {

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
   *
   * @return the name used to build unique element id
   */
  default String getElementId(){
   if(this instanceof Element){
     return ((Element)this).getPresentationMLTag();
   }
   return "";
  }

  /**
   * Convert the label attribute to a label element (MessageML -> PresentationML). It opens also a <div> block, don't forget to close it
   * by calling {@link XmlPrintStream#closeElement()}
   * Normally, you dont need to override the default method
   *
   * @param out
   * @param context
   * @return
   */
  default String labelAsPresentationML(XmlPrintStream out,
      MessageMLContext context){
    out.openElement(Div.MESSAGEML_TAG, Collections.singletonMap(Div.CLASS_ATTR, String.format("%s-group", getElementId())));
    String id = String.format("%s-%s", getElementId(), context.generateShortId());
    out.printElement(getPresentationMLLabelTag(), getAttribute(LABEL), getLabelAttribute(id));
    return id;
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
   * Validate label max length, if present
   */
  default void validateLabel() throws InvalidInputException {
    String labelAttribute = getAttribute(LABEL);
    if(labelAttribute != null && labelAttribute.length() > LABEL_MAX_LENGTH){
      throw new InvalidInputException(String.format(LABEL_TOO_LONG_ERR, LABEL_MAX_LENGTH));
    }
  }

  String getAttribute(String attr);
}
