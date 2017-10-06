package org.symphonyoss.symphony.messageml.markdown.nodes;

import org.commonmark.node.CustomNode;
import org.commonmark.node.Delimited;

public class EmojiNode extends CustomNode implements Delimited {

  private String delimiter;
  private String name;

  public EmojiNode(String delimiter, String name) {
    this.delimiter = delimiter;
    this.name = name;
  }

  @Override
  public String getOpeningDelimiter() {
    return delimiter;
  }

  @Override
  public String getClosingDelimiter() {
    return delimiter;
  }

  public String getName() {
    return name;
  }
}
