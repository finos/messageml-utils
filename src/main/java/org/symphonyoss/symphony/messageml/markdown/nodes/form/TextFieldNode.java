package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class that Represents a Markdown Node for the "TextField" form element.
 *
 * @author Lucas Macedo
 * @since 10/21/2019
 */
public class TextFieldNode extends FormElementNode {
  private final static String MARKDOWN = "Text Field";

  private String placeholder;
  private String initialValue;

  public TextFieldNode(String placeholder, String initialValue) {
    this.placeholder = placeholder;
    this.initialValue = initialValue;
  }

  @Override
  public String getOpeningDelimiter() {
    return LEFT_DELIMITER + MARKDOWN;
  }

  @Override
  public String getText() {
    StringBuilder markdownRepresentation = new StringBuilder();

    if(placeholder != null || initialValue != null) {
      markdownRepresentation.append(":");
    }

    if(placeholder != null) {
      markdownRepresentation.append("[")
          .append(placeholder)
          .append("]");
    }
    
    if(initialValue != null) {
      markdownRepresentation.append(initialValue);
    }

    return markdownRepresentation.toString();
  }
}
