/*
 * Copyright 2016-2017 MessageML - Symphony LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.symphonyoss.symphony.messageml.elements;

import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.commons.lang3.StringUtils;
import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Base class for MessageML elements. Contains methods for constructing MessageML document trees and their
 * PresentationML and Markdown representation, overridden in subclasses if special treatment is required.
 *
 * By default all elements support the "class" and "style" attributes and translate to PresentationML as container elements
 * including their children. To override this behaviour (e.g. to make the element empty), overload the respective
 * methods in subclasses of this class.
 *
 * The intended use of this class is primarily internal. The main entry point are parse() methods in
 * {@link MessageMLContext}.
 * @author lukasz
 * @since 3/27/17
 */
public abstract class Element {
  public static final String CLASS_ATTR = "class";
  public static final String STYLE_ATTR = "style";
  protected static final String ID_ATTR = "id";

  protected FormatEnum format;
  private final Map<String, String> attributes = new LinkedHashMap<>();
  private final List<Element> children = new ArrayList<>();
  private final Element parent;
  private final String messageMLTag;

  Element(Element parent) {
    this(parent, null);
  }

  Element(Element parent, String messageMLTag) {
    this(parent, messageMLTag, FormatEnum.PRESENTATIONML);
  }

  Element(Element parent, String messageMLTag, FormatEnum format) {
    this.messageMLTag = messageMLTag;
    this.parent = parent;
    this.format = format;
  }

  /**
   * Informs if the element has an "id" attribute.
   * The parser that builds all elements keeps track of all ids from the elements that have it, in order to ensure unique values.
   *
   * @return
   */
  public Boolean hasIdAttribute() {
    // An element, by default, should not have it.
    return false;
  }

  /**
   * Process a DOM element, descending into its children, and construct the output MessageML tree.
   */
  public void buildAll(MessageMLParser parser, org.w3c.dom.Element element) throws InvalidInputException,
      ProcessingException {
    NamedNodeMap attr = element.getAttributes();

    for (int i = 0; i < attr.getLength(); i++) {
      buildAttribute(parser, attr.item(i));
    }

    NodeList children = element.getChildNodes();

    for (int i = 0; i < children.getLength(); i++) {
      buildNode(parser, children.item(i));
    }
  }

  /**
   * Parse a DOM attribute into MessageML element properties.
   */
  void buildAttribute(MessageMLParser parser, org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case CLASS_ATTR:
        attributes.put(CLASS_ATTR, getStringAttribute(item));
        break;
      case STYLE_ATTR:
        final String styleAttribute = getStringAttribute(item);
        Styles.validate(styleAttribute);
        attributes.put(STYLE_ATTR, styleAttribute);
        break;
      default:
        if((this instanceof RegexElement && RegexElement.ALL_REGEX_ATTRS.contains(item.getNodeName()))
        || (this instanceof MinMaxLengthElement && MinMaxLengthElement.ALL_MIN_MAX_ATTRS.contains(item.getNodeName()))
        || (format == FormatEnum.MESSAGEML && this instanceof SplittableElement && ((SplittableElement) this).isSplittableNodeComponent(item))){
          attributes.put(item.getNodeName(), getStringAttribute(item));
        } else if(format == FormatEnum.PRESENTATIONML
            && this instanceof SplittableElement
            && ID_ATTR.equals(item.getNodeName())) {
          ((SplittableElement) this).fillAttributes(parser, item, attributes);
        } else {
          throwInvalidInputException(item);
        }
    }
  }

  /**
   * Build a text node or a MessageML element based on the provided DOM node.
   */
  protected void buildNode(MessageMLParser context, org.w3c.dom.Node node) throws InvalidInputException, ProcessingException {
    switch (node.getNodeType()) {
      case org.w3c.dom.Node.TEXT_NODE:
        buildText((Text) node);
        break;

      case org.w3c.dom.Node.ELEMENT_NODE:
        buildElement(context, (org.w3c.dom.Element) node);
        break;

      default:
        throw new InvalidInputException("Invalid element \"" + node.getNodeName() + "\"");

    }
  }

  /**
   * Build a MessageML element based on the provided DOM element and descend into its children.
   */
  private void buildElement(MessageMLParser context, org.w3c.dom.Element element) throws InvalidInputException,
      ProcessingException {
    Element child = context.createElement(element, this);
    if(child != null){
      child.buildAll(context, element);
      child.validate();

      if (child.hasIdAttribute()) {
        context.loadElementId(child.getAttribute(ID_ATTR));
      }

      addChild(child);
    } else if(element.getNodeName().equals(Div.MESSAGEML_TAG)) {
      /*
      When converting from PresentationML -> MessageML tree object some elements are not converted
      like the div generated by SplittableElement (context.createElement(element, this) returns null).
      However, children of this div must not be lost and they must be attached to the current element
       */
      org.w3c.dom.Node node = element.getFirstChild();
      while(node != null){
        buildNode(context, node);
        node = node.getNextSibling();
      }
    }
  }

  /**
   * Build the text content of the element.
   */
  void buildText(Text text) {
    addChild(new TextNode(this, text)); // do not escape here
  }

  /**
   * Traverse the element and its children to construct its representation as a Markdown tree.
   */
  void buildMarkdown(Node parent) throws InvalidInputException {
    for (Element child : this.children) {
      Node node = child.asMarkdown();

      if (node != null) {
        parent.appendChild(node);
      } else {
        node = parent;
      }
      child.buildMarkdown(node);
    }
  }

  /**
   * Traverse the element and its children to construct its representation as EntityJSON nodes.
   */
  void buildEntityJson(ObjectNode parent) {
    for (Element child : this.children) {
      ObjectNode node = child.asEntityJson(parent);

      if (node != null) {
        child.buildEntityJson(node);
      } else {
        child.buildEntityJson(parent);
      }
    }
  }

  /**
   * Return Markdown representation of the element as {@link Node}.
   */
  Node asMarkdown() throws InvalidInputException {
    return null;
  }

  /**
   * Print a PresentationML representation of the element and its children to the provided PrintStream.
   */
  void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    Map<String, String> attributes = new LinkedHashMap<>();

    if(this instanceof RegexElement){
      RegexElement regexElement = (RegexElement)this;
      attributes.putAll(regexElement.getOtherAttributes());
      attributes.putAll(regexElement.getRegexAttrForPresentationML());
    } else {
      attributes.putAll(getAttributes());
    }

    if(this instanceof SplittableElement && ((SplittableElement) this).isSplittable()){
      ((SplittableElement)this).splittableRemove().forEach(remove -> attributes.remove(remove));
      // open div + adding splittable elements
      String uid = ((SplittableElement)this).splittableAsPresentationML(out, context);
      attributes.put("id", uid);
      // render element
      innerAsPresentationML(out, context, attributes);
      // close div
      out.closeElement();
    } else {
      innerAsPresentationML(out, context, attributes);
    }
  }

  private void innerAsPresentationML(XmlPrintStream out,
      MessageMLContext context, Map<String, String> attributes){
    if(areNestedElementsAllowed()){
      out.openElement(getPresentationMLTag(), attributes);
      for (Element child : getChildren()) {
        child.asPresentationML(out, context);
      }
      out.closeElement();
    } else {
      out.printElement(getPresentationMLTag(), attributes);
    }
  }

  /**
   * Return a text representation of the element, descending into its children.
   */
  public String asText() {
    StringBuilder b = new StringBuilder();

    for (Element child : children) {
      b.append(child.asText());
    }

    return b.toString();
  }

  /**
   * Return the EntityJSON representation of the node.
   */
  ObjectNode asEntityJson(ObjectNode parent) {
    return null;
  }

  /**
   * Check the syntax and contents of the element.
   */
  void validate() throws InvalidInputException {
    if(this instanceof RegexElement){
      ((RegexElement)this).validateRegex();
    }
    if(this instanceof SplittableElement){
      ((SplittableElement)this).validateSplittable();
    }
  }

  /**
   * Get a DOM attribute as a String value.
   */
  public String getStringAttribute(org.w3c.dom.Node attribute) {
    return attribute.getTextContent();
  }

  /**
   * Get a DOM attribute as a Long value.
   */
  Long getLongAttribute(org.w3c.dom.Node attribute) throws InvalidInputException {
    String s = getStringAttribute(attribute);

    if (s == null) {
      return null;
    }

    try {
      return Long.parseLong(s);
    } catch (NumberFormatException e) {
      throw new InvalidInputException("Invalid input: " + attribute.getNodeName()
          + " must be a int64 value not \"" + s + "\"");
    }
  }

  /**
   * Get a DOM attribute as a Boolean value.
   */
  Boolean getBooleanAttribute(org.w3c.dom.Node attribute)  {
    String s = getStringAttribute(attribute);

    if (s == null) {
      return null;
    }

    return Boolean.parseBoolean(s);
  }

  /**
   * Get a DOM attribute as a URI.
   */
  URI getUrlAttribute(org.w3c.dom.Node attribute) throws InvalidInputException {
    String s = getStringAttribute(attribute);

    if (s == null) {
      return null;
    }

    try {
      return new URI(s);
    } catch (URISyntaxException e) {
      throw new InvalidInputException("Invalid input: " + attribute.getLocalName()
          + " must be a URI value not \"" + s + "\"");
    }
  }

  public boolean hasExactNumberOfChildren(int childrenNumber) {
    return (getChildren() != null && getChildren().size() == childrenNumber);
  }

  /**
   * Checks if attribute has one of the allowed values
   * This is done in cases we want to enforce specific valid values for attributes.
   *
   * @param attributeName name of attribute that will be checked.
   * @param permittedValues list of values that are allowed for the specified attribute
   * @throws InvalidInputException
   */
  void assertAttributeValue(String attributeName, Collection<String> permittedValues) throws InvalidInputException {
    String attributeValue = getAttribute(attributeName);

    if (!permittedValues.contains(attributeValue.toLowerCase())) {
      throw new InvalidInputException(String.format("Attribute \"%s\" of element \"%s\" can only be one of the following values: [%s].", attributeName,
          this.getMessageMLTag(), String.join(", ", permittedValues)));
    }
  }

  /**
   * Checks if an attribute is not null or empty
   *
   * @param attributeName name of attribute that will be checked.
   * @throws InvalidInputException
   */
  void assertAttributeNotBlank(String attributeName) throws InvalidInputException {
    String attributeValue = getAttribute(attributeName);

    if (attributeValue == null || attributeValue.trim().isEmpty()) {
      throw new InvalidInputException("The attribute \"" + attributeName + "\" is required");
    }
  }

  /**
   * Check that the element is an empty element.
   */
  void assertNoContent() throws InvalidInputException {
    if (!this.getChildren().isEmpty()) {
      throw new InvalidInputException(
          "Element \"" + this.getMessageMLTag() + "\" may not have child elements or text content");
    }
  }

  /**
   * Check that the element has no attributes.
   */
  void assertNoAttributes() throws InvalidInputException {
    if (!this.getAttributes().isEmpty()) {
      throw new InvalidInputException("Element \"" + this.getMessageMLTag() + "\" may not have attributes");
    }
  }

  /**
   * Check that the element has no text content.
   */
  void assertNoText() throws InvalidInputException {
    for (Element child : this.getChildren())
      if (child instanceof TextNode && StringUtils.isNotBlank(((TextNode) child).getText())) {
        throw new InvalidInputException("Element \"" + this.getMessageMLTag() + "\" may not have text content");
      }
  }

  /**
   * Check that the element's children are limited to phrasing content.
   * @throws InvalidInputException
   */
  void assertPhrasingContent() throws InvalidInputException {
    assertContentModel(Arrays.asList(TextNode.class, Link.class, Chime.class, Bold.class, Italic.class, Image.class,
        LineBreak.class, Span.class, Emoji.class, HashTag.class, CashTag.class, Mention.class));
  }

  /**
   * Check that the element's children are limited to allowed element types.
   */
  void assertContentModel(Collection<Class<? extends Element>> permittedChildren) throws InvalidInputException {
    for (Element child : this.getChildren()) {
      if (!permittedChildren.contains(child.getClass())) {

        //Permit whitespace
        if (child instanceof TextNode && StringUtils.isBlank(((TextNode) child).getText())) {
          continue;
        }

        throw new InvalidInputException("Element \"" + child.getMessageMLTag() + "\" is not allowed in \""
            + this.getMessageMLTag() + "\"");
      }
    }
  }

  /**
   * Check that the element's allowed parents are limited to the specified element types.
   */
  void assertParent(Collection<Class<? extends Element>> permittedParents) throws InvalidInputException {
    if (!permittedParents.contains(this.getParent().getClass())) {
      String permittedParentsClassAsString = permittedParents.stream()
          .map(permittedParentClass -> permittedParentClass.getSimpleName().toLowerCase())
          .reduce((item, anotherItem) -> String.format("%s, %s", item, anotherItem))
          .orElse("");
      throw new InvalidInputException(String.format("Element \"%s\" can only be a child of the following elements: [%s]",
          this.getMessageMLTag(), permittedParentsClassAsString));
    }
  }

  /**
   * Check in above levels if an element has a permitted parent.
   */
  void assertParentAtAnyLevel(Collection<Class<? extends Element>> permittedParents) throws InvalidInputException {
    Boolean permittedParentFound = hasParentAtAnyLevel(permittedParents);

    if (!permittedParentFound) {
      String permittedParentsClassAsString = permittedParents.stream()
          .map(permittedParentClass -> permittedParentClass.getSimpleName().toLowerCase())
          .reduce((item, anotherItem) -> String.format("%s, %s", item, anotherItem))
          .orElse("");
      throw new InvalidInputException(String.format("Element \"%s\" can only be a inner child of the following elements: [%s]",
          this.getMessageMLTag(), permittedParentsClassAsString));
    }
  }

  /**
   * Check in above levels if an element has a forbidden parent.
   */
  void assertNotParentAtAnyLevel(Collection<Class<? extends Element>> forbiddenParents) throws InvalidInputException {
    Boolean forbiddenParentFound = hasParentAtAnyLevel(forbiddenParents);

    if (forbiddenParentFound) {
      String forbiddenParentsClassAsString = forbiddenParents.stream()
          .map(forbiddenParentClass -> forbiddenParentClass.getSimpleName().toLowerCase())
          .reduce((item, anotherItem) -> String.format("%s, %s", item, anotherItem))
          .orElse("");
      throw new InvalidInputException(String.format("Element \"%s\" cannot be an inner child of the following elements: [%s]",
          this.getMessageMLTag(), forbiddenParentsClassAsString));
    }
  }

  /**
   * This is to enforce that at least one child of the element has one of the types informed.
   *
   * @param elementTypes list of element types to check
   * @throws InvalidInputException
   */
  void assertContainsChildOfType(Collection<Class<? extends Element>> elementTypes) throws InvalidInputException {
    boolean hasPermittedElementAsChild = this.getChildren().stream()
        .anyMatch(element -> elementTypes.contains(element.getClass()));

    if (!hasPermittedElementAsChild) {
      throw new InvalidInputException(String.format("The \"%s\" element must have at least one child that is any of the following elements: [%s].",
          getMessageMLTag(), getElementsNameByClassName(elementTypes)));
    }
  }

  /**
   * Assert that children with any of the informed types do not exceed the maximum count allowed.
   *
   * @param elementTypes The element types that will be verified in order to ensure that the maximum count was not exceed.
   * @param maxCountPerElementType the maximum quantity, per element type, that is allowed.
   * @throws InvalidInputException
   */
  void assertChildrenNotExceedingMaxCount(Collection<Class<? extends Element>> elementTypes, int maxCountPerElementType) throws InvalidInputException {
    boolean hasExceeded = elementTypes.stream().anyMatch(type -> findElements(type).size() > maxCountPerElementType);
    if (hasExceeded) {
      throw new InvalidInputException(String.format("Element \"%s\" cannot have more than %s children of the following elements: [%s].",
          getElementNameByClass(this.getClass()), maxCountPerElementType, getElementsNameByClassName(elementTypes)));
    }
  }

  /**
   * Return the element's MessageML tag.
   */
  public final String getMessageMLTag() {
    return messageMLTag;
  }

  /**
   * Return the element's PresentationML tag.
   * By default is equals to MessageML tag, override when needed
   */
  public String getPresentationMLTag() {
    return messageMLTag;
  }

  /**
   * Return true if nested elements are allowed
   * By default true, override to false for elements that dont support nested elements
   */
  public boolean areNestedElementsAllowed(){
    return true;
  }

  /**
   * Return a map of the element's attributes.
   */
  public Map<String, String> getAttributes() {
    return attributes;
  }

  /**
   * Return the value of the element's attribute "attr".
   */
  public String getAttribute(String attr) {
    return attributes.get(attr);
  }

  /**
   * Set the element's attribute "attr" to the given value.
   */
  void setAttribute(String attr, String value) {
    attributes.put(attr, value);
  }

  /**
   * Return a list of the element's children.
   */
  public List<Element> getChildren() {
    return children;
  }

  /**
   * Return the nth child of the element.
   */
  public Element getChild(int n) {
    return children.get(n);
  }

  /**
   * Append a child to the element.
   */
  public void addChild(Element child) {
    children.add(child);
  }

  /**
   * Return the number of children of the element.
   */
  public int size() {
    return children.size();
  }

  /**
   * Return the parent of the element.
   */
  public Element getParent() {
    return parent;
  }

  /**
   * Return the format (MessageML or PresentationML) of the element.
   */
  public FormatEnum getFormat() {
    return format;
  }

  /**
   * Search the MessageML tree (depth-first) for elements of a given type.
   * @param type the class of elements to find
   * @return found elements
   */
  public List<Element> findElements(Class<?> type) {
    List<Element> result = new ArrayList<>();
    LinkedList<Element> stack = new LinkedList<>(this.getChildren());

    if (this.getClass() == type) {
      result.add(this);
    }

    while (!stack.isEmpty()) {
      Element child = stack.pop();
      stack.addAll(0, child.getChildren());
      if (child.getClass() == type) {
        result.add(child);
      }
    }

    return result;
  }

  /**
   * Search the MessageML tree (depth-first) for elements with a given MessageML tag.
   * @param tag the MessageML tag of elements to find
   * @return found elements
   */
  public List<Element> findElements(String tag) {
    List<Element> result = new ArrayList<>();
    LinkedList<Element> stack = new LinkedList<>(this.getChildren());

    if (tag.equalsIgnoreCase(this.getMessageMLTag())) {
      result.add(this);
    }

    while (!stack.isEmpty()) {
      Element child = stack.pop();
      stack.addAll(0, child.getChildren());
      if (tag.equalsIgnoreCase(child.getMessageMLTag())) {
        result.add(child);
      }
    }

    return result;
  }

  /**
   * Search the MessageML tree (depth-first) for elements with a given attribute-value pair.
   * @param attribute the attribute name match
   * @param value the attribute value to match
   * @return found elements
   */
  public List<Element> findElements(String attribute, String value) {
    List<Element> result = new ArrayList<>();
    LinkedList<Element> stack = new LinkedList<>(this.getChildren());

    if (value.equals(getAttribute(attribute))) {
      result.add(this);
    }

    while (!stack.isEmpty()) {
      Element child = stack.pop();
      stack.addAll(0, child.getChildren());

      if (value.equals(child.getAttribute(attribute))) {
        result.add(child);
      }
    }

    return result;
  }

  public Integer countNonTextNodesInNodeList(NodeList nodeList) {
    Integer numberOfNonTextNodes = 0;

    for(int i = 0; i < nodeList.getLength(); i ++) {
      if(!"#text".equals(nodeList.item(i).getNodeName())) {
        numberOfNonTextNodes++;
      }
    }

    return numberOfNonTextNodes;
  }

  private String getElementsNameByClassName(Collection<Class<? extends Element>> elementsClasses) {
    return elementsClasses.stream()
        .map(this::getElementNameByClass)
        .collect(Collectors.joining(", "));
  }

  private String getElementNameByClass(Class<? extends Element> element) {
    return element.equals(TextNode.class) ? "text content" : element.getSimpleName().toLowerCase();
  }

  /**
   * Check if the element has one of informed element types as a parent at any level.
   *
   * @param possibleParents
   * @return true if contains; false otherwise.
   */
  private Boolean hasParentAtAnyLevel(Collection<Class<? extends Element>> possibleParents) {
    Element element = this;
    Boolean parentFound = false;

    while (!parentFound && element.getParent() != null) {
      if (possibleParents.contains(element.getParent().getClass())) {
        parentFound = true;
      } else {
        element = element.getParent();
      }
    }
    return parentFound;
  }

  protected void throwInvalidInputException(org.w3c.dom.Node item) throws InvalidInputException {
    throw new InvalidInputException("Attribute \"" + item.getNodeName()
        + "\" is not allowed in \"" + getMessageMLTag() + "\"");
  }
}
