package org.symphonyoss.symphony.messageml.markdown.nodes.form;

public class SelectNode extends FormElementNode {
  private final static String TAG_REPRESENTATION_ON_MARKDOWN = "Dropdown:";
  private final static String RIGHT_DELIMITER = "):";

  public SelectNode(String name) {
    super(TAG_REPRESENTATION_ON_MARKDOWN, name);
  }

  @Override
  public String getClosingDelimiter() {
    return RIGHT_DELIMITER + "\n";
  }
}
