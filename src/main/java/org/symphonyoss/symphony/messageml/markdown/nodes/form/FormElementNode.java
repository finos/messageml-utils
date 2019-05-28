package org.symphonyoss.symphony.messageml.markdown.nodes.form;

import org.commonmark.node.CustomBlock;

/**
 * Class implemented to have the default spec for markdown parsing of most Symphony Elements
 * @author Cristiano Faustino
 * @since 05/28/2019
 */
public class FormElementNode extends CustomBlock {
  private final static String LEFT_DELIMITER = "(";
  private final static String RIGHT_DELIMITER = ")";

  private String tagRepresentationOnMarkdown;
  private String text;

  public FormElementNode() {
    // Do nothing
  }

  public FormElementNode(String tagRepresentationOnMarkdown, String text) {
    this.tagRepresentationOnMarkdown = tagRepresentationOnMarkdown;
    this.text = text;
  }

  public FormElementNode(String tagRepresentationOnMarkdown) {
    this.tagRepresentationOnMarkdown = tagRepresentationOnMarkdown;
  }

  public String getOpeningDelimiter() {
    return LEFT_DELIMITER + tagRepresentationOnMarkdown;
  }

  public String getClosingDelimiter() {
    return RIGHT_DELIMITER + "\n";
  }

  public String getText() {
    return text;
  }

  public void setText(String text) {
    this.text = text;
  }
}
