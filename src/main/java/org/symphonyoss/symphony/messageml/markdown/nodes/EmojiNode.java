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

  public EmojiNode(String shortcode) {
    this();
    this.shortcode = shortcode;
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

}
