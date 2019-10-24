package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class that Represents a Markdown Node for the "DateSelector" form element.
 *
 * @author Lucas Macedo
 * @since 10/21/2019
 */
public class DateSelectorNode extends FormElementNode {
  private final static String MARKDOWN = "Date Selector";

  private String placeholder;

  public DateSelectorNode(String placeholder) {
    this.placeholder = placeholder;
  }

  @Override
  public String getOpeningDelimiter() {
    return LEFT_DELIMITER + MARKDOWN;
  }

  @Override
  public String getText() {
    return (placeholder != null) ? String.format(":[%s]", placeholder) : "";
  }
}
