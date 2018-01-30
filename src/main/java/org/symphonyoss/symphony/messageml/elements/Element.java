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

/**
 * Base class for MessageML elements. Contains methods for constructing MessageML document trees and their
 * PresentationML and Markdown representation, overridden in subclasses if special treatment is required.
 *
 * By default all elements support the "class" attribute and translate to PresentationML as container elements
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
  protected FormatEnum format = FormatEnum.PRESENTATIONML;
  private final Map<String, String> attributes = new LinkedHashMap<>();
  private final List<Element> children = new ArrayList<>();
  private final Element parent;
  private String messageMLTag;

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
   * Process a DOM element, descending into its children, and construct the output MessageML tree.
   */
  public void buildAll(MessageMLParser context, org.w3c.dom.Element element) throws InvalidInputException,
      ProcessingException {
    NamedNodeMap attr = element.getAttributes();

    for (int i = 0; i < attr.getLength(); i++) {
      buildAttribute(attr.item(i));
    }

    NodeList children = element.getChildNodes();

    for (int i = 0; i < children.getLength(); i++) {
      buildNode(context, children.item(i));
    }
  }

  /**
   * Parse a DOM attribute into MessageML element properties.
   */
  void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case CLASS_ATTR:
        attributes.put(CLASS_ATTR, getStringAttribute(item));
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  /**
   * Build a text node or a MessageML element based on the provided DOM node.
   */
  private void buildNode(MessageMLParser context, org.w3c.dom.Node node) throws InvalidInputException, ProcessingException {
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
    child.buildAll(context, element);
    child.validate();
    addChild(child);
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
  void asPresentationML(XmlPrintStream out) {
    out.openElement(getMessageMLTag(), getAttributes());

    for (Element child : getChildren()) {
      child.asPresentationML(out);
    }

    out.closeElement();
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
  }

  /**
   * Get a DOM attribute as a String value.
   */
  String getStringAttribute(org.w3c.dom.Node attribute) {
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
      throw new InvalidInputException("Invalid input: " + attribute.getLocalName()
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
        LineBreak.class, Span.class, Emoji.class));
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
      throw new InvalidInputException("Element \"" + this.getMessageMLTag() + "\" is not allowed as a child of \""
          + this.getParent().getMessageMLTag() + "\"");
    }
  }

  /**
   * Return the element's MessageML tag.
   */
  public String getMessageMLTag() {
    return messageMLTag;
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

}
