package org.symphonyoss.symphony.messageml.markdown.nodes.form;

import org.apache.commons.lang3.StringUtils;
import org.commonmark.node.CustomBlock;

/**
 * Class implemented to have the default spec for markdown parsing of most Symphony Elements
 * @author Cristiano Faustino
 * @since 05/28/2019
 */
public class FormElementNode extends CustomBlock {
  protected final static String LEFT_DELIMITER = "(";
  protected final static String RIGHT_DELIMITER = ")";

  private String tagRepresentationOnMarkdown;
  private String text;

  public FormElementNode() {
    // Do nothing
  }

  public FormElementNode(String tagRepresentationOnMarkdown, String text) {
    this.tagRepresentationOnMarkdown = tagRepresentationOnMarkdown;
    this.text = addEscapeCharacter(text);
  }

  public FormElementNode(String tagRepresentationOnMarkdown) {
    this.tagRepresentationOnMarkdown = tagRepresentationOnMarkdown;
    this.text = "";
  }

  public String getOpeningDelimiter() {
    return LEFT_DELIMITER + tagRepresentationOnMarkdown;
  }

  public String getClosingDelimiter() {
    return RIGHT_DELIMITER;
  }

  public String getText() {
    return text;
  }

  /**
   * Escape reserved Markdown characters which are part of the message content, to prevent them from being interpreted
   * as Markdown
   */
  public String addEscapeCharacter(String content) {
    return StringUtils.replaceEach(content, new String[]{"_","*","-","+","`"}, new String[]{"\\_","\\*","\\-","\\+","\\`"});
  }


}
