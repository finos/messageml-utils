package org.symphonyoss.symphony.messageml.markdown.nodes.form;

import org.commonmark.node.HardLineBreak;

public class DialogNode extends FormNode {
  private final static String MARKDOWN = "**Dialog**";
  private final static String DIALOG_DELIMITER = "---";

  public DialogNode() {
    super();
  }

  @Override
  public String getOpeningDelimiter() {
    return DIALOG_DELIMITER + "\n" + MARKDOWN + "\n";
  }

  @Override
  public String getClosingDelimiter() {
    return DIALOG_DELIMITER + "\n";
  }

  @Override
  public void appendChild(org.commonmark.node.Node node) {
    super.appendChild(node);
    super.appendChild(new HardLineBreak()); // add a line break between each child
  }
}
