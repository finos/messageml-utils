package org.symphonyoss.symphony.messageml.markdown.nodes;

import org.commonmark.node.CustomNode;
import org.commonmark.node.Delimited;
import org.commonmark.node.Visitor;

public class EmojiNode extends CustomNode implements Delimited {

  public static final char DELIMITER = ':';

  private String delimiter;
  private String annotation;

  public EmojiNode(String annotation) {
    this();
    this.annotation = annotation;
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

  public String getAnnotation() {
    return this.annotation;
  }

  public void setAnnotation(String name) {
    this.annotation = annotation;
  }

}
