package org.symphonyoss.symphony.messageml.elements;

import org.apache.commons.lang3.tuple.Pair;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * @author enrico.molino (24/06/2020)
 *
 * This interface is used internally by the extending ones {@link LabelableElement} {@link TooltipableElement}
 * It is used by all element interfaces that require, during the conversion MessageML to PresentationML, to generate multiple
 * elements starting by a single one.
 *
 * BEWARE: dont implement this interface, instead implement children interfaces
 *
 * E.g.:
 *
 * <element-implementing-splittable attribute1="1">value</element-implementing-splittable>
 *
 * to:
 *
 * <div class="element-implementing-splittable-group">
 *  <attribute1>1</attribute1>
 *  <element>value</element>
 * </div>
 *
 */
public interface SplittableElement {

  String PRESENTATIONML_DIV_FLAG = "data-generated";

  /**
   * Convert the splittable elements (MessageML -> PresentationML). It opens also a <div> block, don't forget to close it
   * by calling {@link XmlPrintStream#closeElement()}
   * Normally, you dont need to override the default method
   *
   * @return the unique id generated
   */
  default String splittableAsPresentationML(XmlPrintStream out,
      MessageMLContext context){
    Map<String, String> attributes = new LinkedHashMap<>();
    attributes.put(Div.CLASS_ATTR, String.format("%s-group", getElementId()));
    attributes.put(PRESENTATIONML_DIV_FLAG, Boolean.TRUE.toString());
    out.openElement(Div.MESSAGEML_TAG, attributes);
    String id = String.format("%s-%s", getElementId(), context.generateShortId());

    // it this method has been called, it is expected to have a label or a tooltip or both
    if(this instanceof LabelableElement){
      LabelableElement labelableElement = (LabelableElement)this;
      if(labelableElement.isLabel()) {
        out.printElement(labelableElement.getPresentationMLLabelTag(),
            getAttribute(LabelableElement.LABEL), labelableElement.getLabelAttribute(id));
      }
    }
    if(this instanceof TooltipableElement){
      TooltipableElement tooltipableElement = (TooltipableElement)this;
      if(tooltipableElement.isTooltip()) {
        out.openElement(tooltipableElement.getPresentationMLTooltipTag(),
            tooltipableElement.getTooltipAttributes(id));
        out.closeElement();
      }
    }
    return id;
  }

  /**
   *
   * @return attributes that should not present in PresentationML main element
   *
   * Normally, you dont need to override the default method
   */
  default Set<String> splittableRemove(){
    Set<String> result = new LinkedHashSet<>();
    if(this instanceof LabelableElement){
      result.add(LabelableElement.LABEL);
    }
    if(this instanceof TooltipableElement){
      result.add(TooltipableElement.TITLE);
    }
    return result;
  }

  /**
   * Check if the current PresentationML node is part of SplittableElement
   * Normally, you dont need to override the default method
   */
  default boolean isSplittableNodeComponent(org.w3c.dom.Node item){
    if(this instanceof LabelableElement){
      if(((LabelableElement) this).isLabelNode(item)){
        return true;
      }
    }
    if(this instanceof TooltipableElement){
      if(TooltipableElement.isTooltipNode(item)){
        return true;
      }
    }
    return false;
  }

  /**
   * Validate all splittable elements present
   * Normally, you dont need to override the default method
   *
   * @throws InvalidInputException
   */
  default void validateSplittable() throws InvalidInputException {
    if(this instanceof LabelableElement){
      ((LabelableElement) this).validateLabel();
    }
    if(this instanceof TooltipableElement){
      ((TooltipableElement) this).validateTooltip();
    }
  }

  /**
   * @return true if any splittable element exists
   * Normally, you dont need to override the default method
   */
  default boolean isSplittable(){
    if(this instanceof LabelableElement){
      if(((LabelableElement) this).isLabel()){
        return true;
      }
    }
    if(this instanceof TooltipableElement){
      if(((TooltipableElement) this).isTooltip()){
        return true;
      }
    }
    return false;
  }

  /**
   * @return the name used to build unique element id
   */
  default String getElementId(){
    if(this instanceof Element){
      return ((Element)this).getPresentationMLTag();
    }
    return "";
  }

  /**
   * Fill map attributes passed as parameter, used internally during parsing Normally, you dont need
   * to override the default method
   */
  default void fillAttributes(MessageMLParser parser, org.w3c.dom.Node item,
      Map<String, String> attributes)
      throws InvalidInputException {
    String id = getStringAttribute(item);
    Optional<Map<Class<? extends SplittableElement>, Map<String, String>>> allSplittableAttributes =
        parser.getAllSplittableAttributes(id);
    if (allSplittableAttributes.isPresent()) {
      for (Map<String, String> splittableAttributes : allSplittableAttributes.get().values()) {
        attributes.putAll(splittableAttributes);
      }
    }
    Optional<Map<Class<? extends SplittableElement>, Pair<String, String>>> allSplittableValues =
        parser.getAllSplittableValues(id);
    if (allSplittableValues.isPresent()) {
      for (Pair<String, String> splittableValue : allSplittableValues.get().values()) {
        attributes.put(splittableValue.getKey(), splittableValue.getValue());
      }
    }
  }

  /**
   * Fill this {@link Element} map attributes, used internally during parsing
   * Normally, you dont need to override the default method
   */
  default void fillAttributes(MessageMLParser parser, org.w3c.dom.Node item)
      throws InvalidInputException {
    fillAttributes(parser, item, getAttributes());
  }

  /**
   * Normally implemented by the root {@link Element} implementing this interface
   */
  String getAttribute(String attr);

  /**
   * Normally implemented by the root {@link Element} implementing this interface
   */
  Map<String, String> getAttributes();

  /**
   * Normally implemented by the root {@link Element} implementing this interface
   */
  String getStringAttribute(org.w3c.dom.Node attribute);
}
