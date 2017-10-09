package org.symphonyoss.symphony.messageml.markdown.nodes;

import org.commonmark.node.CustomNode;
import org.commonmark.node.Delimited;
import org.commonmark.node.Visitor;

public class EmojiNode extends CustomNode implements Delimited {

  public static final char DELIMITER = ':';

  private String delimiter;
  private String name;

  public EmojiNode(String name) {
    this();
    this.name = name;
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

  public String getName() {
    return this.name;
  }

  public void setName(String name) {
    this.name = name;
  }

}
