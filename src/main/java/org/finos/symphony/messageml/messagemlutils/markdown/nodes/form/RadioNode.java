package org.finos.symphony.messageml.messagemlutils.markdown.nodes.form;

import org.apache.commons.lang3.StringUtils;

/**
 * Class that Represents a Markdown Node for the "Radio" form element.
 *
 * @author Lucas Macedo
 * @since 10/21/2019
 */
public class RadioNode extends FormElementNode {
  private final static String MARKDOWN = "Radio Button";
  private final static String RADIO_DELIMITER = " ";

  private String label;
  
  public RadioNode() {
    super(MARKDOWN);
  }

  public RadioNode(String label) {
    super(MARKDOWN, label);
    this.label = label;
  }

  @Override
  public String getOpeningDelimiter() {
    return RADIO_DELIMITER;
  }

  @Override
  public String getClosingDelimiter() {
    return RADIO_DELIMITER;
  }
  
  @Override
  public String getText() {
    return StringUtils.defaultIfBlank(label, StringUtils.EMPTY);
  }
}
