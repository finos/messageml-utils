package org.symphonyoss.symphony.messageml.markdown.nodes;

import org.commonmark.node.CustomBlock;

public class FormNode extends CustomBlock {
  private final static String LEAD = "Form (log into desktop client to answer):";
  private final static String DELIMITER = "---";

  public String getOpeningDelimiter() {
    return LEAD + "\n" + DELIMITER + "\n";
  }

  public String getClosingDelimiter() {
    return "\n" + DELIMITER + "\n";
  }
}
