package org.symphonyoss.symphony.messageml.markdown.nodes.form;

public class TextFieldNode extends FormElementNode {
  private final static String TAG_REPRESENTATION_ON_MARKDOWN = "Text Field:";
  
  public TextFieldNode() {
    super(TAG_REPRESENTATION_ON_MARKDOWN);
  }

  @Override
  public String getClosingDelimiter() {
    return RIGHT_DELIMITER;
  }
}
