package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class that Represents a Markdown Node for the "Select" form element.
 *
 * @author Cristiano Faustino
 * @since 06/05/2019
 */
public class SelectNode extends FormElementNode {
  private final static String MARKDOWN = "Dropdown";
  private final static String RIGHT_DELIMITER = "):";
  
  private String placeholder;

  public SelectNode(String placeholder) {
    super(MARKDOWN, placeholder);
    this.placeholder = placeholder;
  }
  
  @Override
  public String getClosingDelimiter() {
    return RIGHT_DELIMITER + "\n";
  }

  @Override
  public String getText() {
    return (placeholder != null) ? String.format(":[%s]", placeholder) : "";
  }
}
