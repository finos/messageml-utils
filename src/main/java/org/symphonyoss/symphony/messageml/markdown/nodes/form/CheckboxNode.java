package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class that Represents a Markdown Node for the "Checkbox" form element.
 *
 * @author Lucas Macedo
 * @since 10/21/2019
 */
public class CheckboxNode extends FormElementNode {
  private final static String MARKDOWN = "Checkbox";

  private String label;
  
  public CheckboxNode() {
  }

  public CheckboxNode(String label) {
    this.label = label;
  }

  @Override
  public String getOpeningDelimiter() {
    return LEFT_DELIMITER + MARKDOWN;
  }

  @Override
  public String getText() {
    return (label != null) ? ":" + label : "";
  }
}
