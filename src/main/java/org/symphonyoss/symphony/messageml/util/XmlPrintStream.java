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

  private static final String STANDARD_ATTRIBUTE_PATTERN = " %s=\"%s\"";
  private static final String JSON_ATTRIBUTE_PATTERN = " %s='%s'";

  /**
   * Constructor.
   * @param outputStream An OutputStream to which the formatted output will be sent.
   */
  public XmlPrintStream(OutputStream outputStream) {
    super(outputStream);
  }

  private void startElement(String name, Object... attributes) {
    println("<" + name);
    indent();

    int i = 0;

    while (i < attributes.length) {
      if (i < attributes.length - 1) {
        printAttribute(attributes[i++].toString(), attributes[i++]);
      } else {
        printAttribute(attributes[i++].toString(), null);
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
   * the element. Normally you can use {@code Map<String,String>} but if you want specify the attribute format use
   * {@code Map<String, XMLAttribute>}. In particular, attributes containing json can be processed to avoid escaping of double quote, but
   * wrapping the attribute in a single quote
   */
  public void openElement(String name, Map<?, ?> attributes) {
    elementStack.push(name);
    print("<" + name);

    for (Entry<?, ?> entry : attributes.entrySet()) {
      printAttribute(entry.getKey(), entry.getValue());
    }
    if (this.isNoNl()) {
      print(">");
    } else {
      println(">");
    }
    indent();
  }

  /**
   * Open an XML element with the given name, and attributes. A call to closeElement() will output
   * the appropriate XML closing tag. This class remembers the tag names.
   *
   * The String parameters are taken to be alternatively names and values. Any odd value
   * at the end of the list is added as a valueless attribute.
   * @param name Name of the element.
   * @param attributes Attributes in name value pairs. Normally you can use pair of String,String but
   * if you want specify the attribute format use pair of String, XMLAttribute. In particular, attributes containing json can be processed to
   * avoid escaping of double quote, but wrapping the attribute in a single quote
   */
  public void openElement(String name, Object... attributes) {
    elementStack.push(name);
    startElement(name, attributes);
    println(">");

  }

  /**
   * Close an element previously created with openElement().
   */
  public void closeElement() {
    outdent();
    if (this.isNoNl()) {
      print("</" + elementStack.pop() + ">");
    } else {
      println("</" + elementStack.pop() + ">");
    }
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
   * @param attributes Alternate names and values of attributes for the element. Normally you can
   * use pair of String,String but if you want specify the attribute format use pair of String, XMLAttribute. In
   * particular, attributes containing json can be processed to
   * avoid escaping of double quote, but wrapping the attribute in a single quote
   */
  public void printElement(String name, String value, Object... attributes) {
    startElement(name, attributes);
    if (value != null) { println(">" + escape(value) + "</" + name + ">"); } else { println("/>"); }
    outdent();
  }

  /**
   * Output a complete element with the given attributes.
   * @param elementName Name of element.
   * @param attributes A map of name value pairs which will be used to add attributes to
   * the element. Normally you can use {@code Map<String,String>} but if you want specify the
   * attribute format use {@code Map<String, XMLAttribute>}. In particular, attributes containing json can be
   * processed to avoid escaping of double quote, but wrapping the attribute in a single quote
   */
  public void printElement(String elementName, Map<?, ?> attributes) {
    printElement(elementName, null, attributes);
  }

  /**
   * Output a complete element with the given content and attributes.
   * @param elementName Name of element.
   * @param value Content of element.
   * @param attributes A map of name value pairs which will be used to add attributes to
   * the element. Normally you can use {@code Map<String,String>} but if you want specify the
   * attribute format use {@code Map<String, XMLAttribute>}. In particular, attributes containing json can be
   * processed to avoid escaping of double quote, but wrapping the attribute in a single quote
   */
  public void printElement(String elementName, String value, Map<?, ?> attributes) {
    print("<" + elementName);

    for (Entry<?, ?> entry : attributes.entrySet()) {
      printAttribute(entry.getKey(), entry.getValue());
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
    return escape(in, XMLAttribute.Format.STANDARD);
  }

  /**
   * Translate reserved XML characters to XML entities.
   * @param in Input string.
   * @param format Input format. The Json format does not escape ", but it escapes ' !
   */
  public String escape(String in, XMLAttribute.Format format) {
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
          if(XMLAttribute.Format.JSON.equals(format)){
            out.append(c);
          } else {
            out.append("&quot;");
          }
          break;
        case '\'':
          if(XMLAttribute.Format.JSON.equals(format)){
            out.append("&apos;");
          } else {
            out.append(c);
          }
          break;
        default:
          out.append(c);
      }
    }

    return out.toString();
  }

  /**
   * Replace multiple newline characters with a single space.
   * @param textContent input String
   */
  public static String removeNewLines(String textContent) {
    if (textContent == null) {
      return "";
    }

    StringBuilder s = new StringBuilder();
    boolean inNl = false;

    for (char c : textContent.toCharArray()) {
      if (c == '\n') {
        if (!inNl) {
          s.append(' ');
          inNl = true;
        }
      } else {
        inNl = false;
        s.append(c);
      }
    }
    return s.toString();
  }

  private void printAttribute(Object attrName, Object attrValue){
    if(attrValue == null){
      println(" " + attrName);
    } else {
      XMLAttribute.Format format;
      if(attrValue instanceof XMLAttribute){
        format = ((XMLAttribute) attrValue).getFormat();
      } else {
        format = XMLAttribute.Format.STANDARD;
      }
      if(XMLAttribute.Format.JSON.equals(format)){
        print(String.format(JSON_ATTRIBUTE_PATTERN, attrName, escape(attrValue.toString(), format)));
      } else {
        // Standard attribute, wrapped by a double quote
        print(String.format(STANDARD_ATTRIBUTE_PATTERN, attrName, escape(attrValue.toString(), format)));
      }
    }
  }
}
