package org.symphonyoss.symphony.messageml.markdown.nodes;

import org.commonmark.node.CustomNode;
import org.commonmark.node.Delimited;

public class EmojiNode extends CustomNode implements Delimited {

  private String delimiter;

  public EmojiNode(String delimiter) {
    this.delimiter = delimiter;
  }

  @Override
  public String getOpeningDelimiter() {
    return delimiter;
  }

  @Override
  public String getClosingDelimiter() {
    return delimiter;
  }
}
