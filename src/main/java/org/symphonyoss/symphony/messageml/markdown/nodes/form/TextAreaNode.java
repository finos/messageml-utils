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
    super(MARKDOWN);
    this.placeholder = placeholder;
    this.initialValue = initialValue;
  }
  
  @Override
  public String getText() {
    StringBuilder markdownRepresentation = new StringBuilder();
    
    if(placeholder != null || initialValue != null) {
      markdownRepresentation.append(":");
    }
      
    if(placeholder != null) {
      markdownRepresentation.append("[")
          .append(placeholder)
          .append("]");
    }
    
    if(initialValue != null) {
      markdownRepresentation.append(initialValue);
    }

    return markdownRepresentation.toString();
  }
}
