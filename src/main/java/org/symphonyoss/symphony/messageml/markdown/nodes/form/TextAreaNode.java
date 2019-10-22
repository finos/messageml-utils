package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class that Represents a Markdown Node for the "TextArea" form element.
 *
 * @author Lucas Macedo
 * @since 10/21/2019
 */
public class TextAreaNode extends FormElementNode {
  private final static String MARKDOWN = "Text Area";

  private String placeholder;
  private String initialValue;

  public TextAreaNode(String placeholder, String initialValue) {
    this.placeholder = placeholder;
    this.initialValue = initialValue;
  }

  @Override
  public String getOpeningDelimiter() {
    return LEFT_DELIMITER + MARKDOWN;
  }

  @Override
  public String getText() {
    String text = ((placeholder != null) ? "[" + placeholder + "]" : "") +
        ((initialValue != null) ? initialValue : "") ;

    return (!text.isEmpty()) ? ":" + text : "";
  }
}
