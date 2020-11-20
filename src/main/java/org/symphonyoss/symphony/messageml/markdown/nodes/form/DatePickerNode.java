package org.symphonyoss.symphony.messageml.markdown.nodes.form;

public class DatePickerNode extends FormElementNode {
  private final static String MARKDOWN = "Date Picker";

  private String placeholder;

  public DatePickerNode(String placeholder) {
    super(MARKDOWN, placeholder);
    this.placeholder = placeholder;
  }

  @Override
  public String getText() {
    return (placeholder != null) ? String.format(":[%s]", placeholder) : "";
  }

}
