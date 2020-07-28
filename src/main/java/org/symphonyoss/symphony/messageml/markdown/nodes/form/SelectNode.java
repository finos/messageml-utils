package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class that Represents a Markdown Node for the "Select" form element.
 *
 * @author Cristiano Faustino
 * @since 06/05/2019
 */
public class SelectNode extends FormElementNode implements PlaceholderLabelTooltipNode {
  private final static String MARKDOWN = "Dropdown";
  private final static String RIGHT_DELIMITER = "):";
  
  private String placeholder;
  private String label;
  private String tooltip;

  public SelectNode(String placeholder, String label, String tooltip) {
    super(MARKDOWN, placeholder);
    this.placeholder = placeholder;
    this.label = label;
    this.tooltip = tooltip;
  }
  
  @Override
  public String getClosingDelimiter() {
    return RIGHT_DELIMITER + "\n";
  }

  @Override
  public String getText() {
    return generateMarkdownPlaceholderLabelAndTooltip(placeholder, label, tooltip);
  }
}
