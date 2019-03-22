package org.symphonyoss.symphony.messageml.markdown.nodes;

import org.commonmark.node.CustomNode;
import org.commonmark.node.Delimited;
import org.commonmark.node.Visitor;

public class OptionNode extends CustomNode implements Delimited {
  private final static String LEFT_DELIMITER = "-";
  private final static String RIGHT_DELIMITER = "";

  @Override
  public String getOpeningDelimiter() {
    return LEFT_DELIMITER;
  }

  @Override
  public String getClosingDelimiter() {
    return RIGHT_DELIMITER + "\n";
  }

  @Override
  public void accept(Visitor visitor) {
    visitor.visit(this);
  }
}
