package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class that Represents a Markdown Node for the "Select" form element.
 * - The "LEFT_DELIMITER" for this one is the default one from the {@link FormElementNode} class.
 *
 * @author Cristiano Faustino
 * @since 06/05/2019
 */
public class SelectNode extends FormElementNode {
  private final static String MARKDOWN = "Dropdown:";
  private final static String RIGHT_DELIMITER = "):";

  public SelectNode(String name) {
    super(MARKDOWN, name);
  }

  @Override
  public String getClosingDelimiter() {
    return RIGHT_DELIMITER + "\n";
  }
}
