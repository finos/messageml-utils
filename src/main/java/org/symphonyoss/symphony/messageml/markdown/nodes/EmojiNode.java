package org.symphonyoss.symphony.messageml.markdown.nodes;

import org.commonmark.node.CustomNode;
import org.commonmark.node.Delimited;
import org.commonmark.node.Visitor;

/**
 * Class representing a Markdown node for emojis.
 *
 * @author cristiadu
 * @since 10/10/17
 */

public class EmojiNode extends CustomNode implements Delimited {

  public static final char DELIMITER = ':';

  private String delimiter;
  private String shortcode;
  @Deprecated
  private String annotation;

  public EmojiNode(String shortcode) {
    this();
    this.shortcode = shortcode;
    this.annotation = shortcode;
  }

  public EmojiNode() {
    this.delimiter = String.valueOf(DELIMITER);
  }

  @Override
  public String getOpeningDelimiter() {
    return delimiter;
  }

  @Override
  public String getClosingDelimiter() {
    return delimiter;
  }

  @Override
  public void accept(Visitor visitor) {
    visitor.visit(this);
  }

  public String getShortcode() {
    return this.shortcode;
  }

  public void setShortcode(String shortcode) {
    this.shortcode = shortcode;
  }

  /**
   * It is deprecated, use getShortcode() instead
   * @return annotation
   */
  @Deprecated
  public String getAnnotation() {
    return this.annotation;
  }

  /**
   * It is deprecated, use setShortcode() instead
   * @param name
   */
  @Deprecated
  public void setAnnotation(String name) {
    this.annotation = annotation;
  }

}
