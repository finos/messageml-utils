/* ----------------------------------------------------------------------------
 * Copyright (C) 2016
 * Symphony Communication Services, LLC
 * All Rights Reserved
 * ---------------------------------------------------------------------------- */

package org.symphonyoss.symphony.messageml.util;

import java.io.OutputStream;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;


/**
 * A PrintStream based on IndentedPrintStream which adds functions to format XML.
 * @author Bruce Skingle
 */
public class XmlPrintStream extends IndentedPrintStream {
  private final Stack<String> elementStack = new Stack<>();

  /**
   * Constructor.
   * @param outputStream An OutputStream to which the formatted output will be sent.
   */
  public XmlPrintStream(OutputStream outputStream) {
    super(outputStream);
  }

  private void startElement(String name, String... attributes) {
    println("<" + name);
    indent();

    int i = 0;

    while (i < attributes.length) {
      if (i < attributes.length - 1) { println(" " + attributes[i++] + "=\"" + attributes[i++] + "\""); } else {
        println(" " + attributes[i++]);
      }
    }
  }

  /**
   * Open an XML element with the given name. A call to closeElement() will output
   * the appropriate XML closing tag. This class remembers the tag names.
   * @param name Name of the XML element to open.
   */
  public void openElement(String name) {
    elementStack.push(name);
    println("<" + name + ">");
    indent();
  }

  /**
   * Open an XML element with the given name, and attributes. A call to closeElement() will output
   * the appropriate XML closing tag. This class remembers the tag names.
   * @param name Name of the XML element to open.
   * @param attributes A map of name value pairs which will be used to add attributes to
   * the element.
   */
  public void openElement(String name, Map<String, String> attributes) {
    elementStack.push(name);
    print("<" + name);

    for (Entry<String, String> entry : attributes.entrySet()) {
      print(" " + entry.getKey() + "=\"" + escape(entry.getValue()) + "\"");
    }
    println(">");
    indent();
  }

  /**
   * Open an XML element with the given name, and attributes. A call to closeElement() will output
   * the appropriate XML closing tag. This class remembers the tag names.
   *
   * The String parameters are taken to be alternatively names and values. Any odd value
   * at the end of the list is added as a valueless attribute.
   * @param name Name of the element.
   * @param attributes Attributes in name value pairs.
   */
  public void openElement(String name, String... attributes) {
    elementStack.push(name);
    startElement(name, attributes);
    println(">");

  }

  /**
   * Close an element previously created with openElement().
   */
  public void closeElement() {
    outdent();
    println("</" + elementStack.pop() + ">");
  }

  /**
   * Output a complete element with the given content.
   * @param elementName Name of element.
   * @param value Content of element.
   */
  public void printElement(String elementName, Object value) {
    println("<" + elementName + ">" + (value == null ? "" : escape(value.toString())) + "</" + elementName + ">");
  }

  /**
   * Output an element with the given content (value). The opening and closing tags are
   * output in a single operation.
   * @param name Name of the element.
   * @param value Contents of the element.
   * @param attributes Alternate names and values of attributes for the element.
   */
  public void printElement(String name, String value, String... attributes) {
    startElement(name, attributes);
    if (value != null) { println(">" + escape(value) + "</" + name + ">"); } else { println("/>"); }
    outdent();
  }

  /**
   * Output a complete element with the given content and attributes.
   * @param elementName Name of element.
   * @param value Content of element.
   * @param attributes A map of name value pairs which will be used to add attributes to
   * the element.
   */
  public void printElement(String elementName, String value, Map<String, String> attributes) {
    print("<" + elementName);

    for (Entry<String, String> entry : attributes.entrySet()) {
      print(" " + entry.getKey() + "=\"" + escape(entry.getValue()) + "\"");
    }

    if (value != null) {
      println(">" + escape(value) + "</" + elementName + ">");
    } else {
      println("/>");
    }
  }

  /**
   * Output a complete empty element.
   * @param name Name of element.
   */
  public void printElement(String name) {
    println("<" + name + "/>");
  }

  /**
   * Output a comment.
   * @param comment Comment text.
   */
  public void printComment(String comment) {
    println("<!-- " + comment + " -->");
  }

  /**
   * Translate reserved XML characters to XML entities.
   * @param in Input string.
   */
  public String escape(String in) {
    StringBuffer out = new StringBuffer();

    for (char c : in.toCharArray()) {
      switch (c) {
        case '<':
          out.append("&lt;");
          break;
        case '>':
          out.append("&gt;");
          break;
        case '&':
          out.append("&amp;");
          break;
        case '"':
          out.append("&quot;");
          break;
        default:
          out.append(c);
      }
    }

    return out.toString();
  }
}
