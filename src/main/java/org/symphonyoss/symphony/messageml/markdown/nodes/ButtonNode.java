package org.symphonyoss.symphony.messageml.markdown.nodes;

import org.commonmark.node.CustomBlock;

public class ButtonNode extends CustomBlock {
  private final static String LEAD = "Button:";
  private final static String LEFT_DELIMITER = "(";
  private final static String RIGHT_DELIMITER = ")";

  public String getOpeningDelimiter() {
    return LEFT_DELIMITER + LEAD;
  }

  public String getClosingDelimiter() {
    return RIGHT_DELIMITER;
  }
}
