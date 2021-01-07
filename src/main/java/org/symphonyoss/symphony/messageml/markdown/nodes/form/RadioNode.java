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
    super(MARKDOWN);
  }

  public RadioNode(String label) {
    super(MARKDOWN, label);
    this.label = addEscapeCharacter(label);
  }
  
  @Override
  public String getText() {
    return (label != null) ? ":" + label : "";
  }
}
