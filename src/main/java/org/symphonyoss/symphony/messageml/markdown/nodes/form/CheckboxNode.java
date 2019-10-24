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
    super(MARKDOWN);
  }

  public CheckboxNode(String label) {
    super(MARKDOWN);
    this.label = label;
  }
  
  @Override
  public String getText() {
    return (label != null) ? ":" + label : "";
  }
}
