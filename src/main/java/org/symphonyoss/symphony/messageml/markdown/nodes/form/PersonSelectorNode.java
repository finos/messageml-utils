package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class that Represents a Markdown Node for the "PersonSelector" form element.
 *
 * @author Lucas Macedo
 * @since 10/21/2019
 */
public class PersonSelectorNode extends FormElementNode {
  private final static String MARKDOWN = "Person Selector";

  private String placeholder;

  public PersonSelectorNode(String placeholder) {
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
