package org.symphonyoss.symphony.messageml.markdown.nodes.form;

public class ButtonNode extends FormElementNode {
  private final static String MARKDOWN = "Button:";

  public ButtonNode() {
    super(MARKDOWN);
  }

  @Override
  public String getClosingDelimiter() {
    return RIGHT_DELIMITER;
  }
}
