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
  private String label;
  private String tooltip;

  public PersonSelectorNode(String placeholder, String label, String tooltip) {
    super(MARKDOWN, placeholder);
    this.placeholder = placeholder;
    this.label = label;
    this.tooltip = tooltip;
  }
  
  @Override
  public String getText() {
    StringBuilder markdownRepresentation = new StringBuilder();

    if(placeholder != null || label != null || tooltip != null) {
      markdownRepresentation.append(":");
    }

    if(placeholder != null) {
      markdownRepresentation.append("[")
          .append(placeholder)
          .append("]");
    }

    if(label != null) {
      markdownRepresentation.append("[")
          .append(label)
          .append("]");
    }

    if(tooltip != null) {
      markdownRepresentation.append("[")
          .append(tooltip)
          .append("]");
    }

    return markdownRepresentation.toString();
  }
}
