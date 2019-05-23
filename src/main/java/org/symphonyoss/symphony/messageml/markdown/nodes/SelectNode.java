package org.symphonyoss.symphony.messageml.markdown.nodes;

import org.commonmark.node.CustomBlock;

public class SelectNode extends CustomBlock {
  private final static String LEAD = "Dropdown:";
  private final static String LEFT_DELIMITER = "(";
  private final static String RIGHT_DELIMITER = "):";

  private String name;

  public SelectNode(String name) {
    this.name = name;
  }

  public String getOpeningDelimiter() {
    return LEFT_DELIMITER + LEAD;
  }

  public String getClosingDelimiter() {
    return RIGHT_DELIMITER + "\n";
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }
}
