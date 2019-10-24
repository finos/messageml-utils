package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class that Represents a Markdown Node for the "Radio" form element.
 *
 * @author Lucas Macedo
 * @since 10/21/2019
 */
public class RadioNode extends FormElementNode {
  private final static String MARKDOWN = "Radio Button";

  private String label;
  
  public RadioNode() {
    
  }

  public RadioNode(String label) {
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
