package org.symphonyoss.symphony.messageml.markdown.nodes.form;

public class FormNode extends FormElementNode {
  private final static String MARKDOWN = "Form (log into desktop client to answer):";
  private final static String FORM_DELIMITER = "---";

  public FormNode() {
    super(MARKDOWN);
  }

  @Override
  public String getOpeningDelimiter() {
    return MARKDOWN + "\n" + FORM_DELIMITER + "\n";
  }

  @Override
  public String getClosingDelimiter() {
    return "\n" + FORM_DELIMITER + "\n";
  }
}
