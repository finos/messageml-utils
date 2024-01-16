package org.symphonyoss.symphony.messageml.markdown.nodes.form;

import org.apache.commons.lang3.StringUtils;

/**
 * Class that Represents a Markdown Node for the "Select" form element.
 *
 * @author Cristiano Faustino
 * @since 06/05/2019
 */
public class SelectNode extends FormElementNode implements PlaceholderLabelTooltipNode {
  private final static String MARKDOWN = "Dropdown";
  private final static String SELECT_DELIMITER = " ";
  
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
  public String getOpeningDelimiter() {
    return SELECT_DELIMITER;
  }
  @Override
  public String getClosingDelimiter() {
    return SELECT_DELIMITER + "\n";
  }

  public String getText() {
    return StringUtils.defaultIfBlank(label, StringUtils.EMPTY);
  }

}
