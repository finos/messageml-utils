package org.finos.symphony.messageml.messagemlutils.markdown.nodes.form;

public class OptionNode extends FormElementNode {
  private final static String LEFT_OPTION_DELIMITER = "-";
  private final static String RIGHT_OPTION_DELIMITER = "";

  public OptionNode() {
    super();
  }

  @Override
  public String getOpeningDelimiter() {
    return LEFT_OPTION_DELIMITER;
  }

  @Override
  public String getClosingDelimiter() {
    return RIGHT_OPTION_DELIMITER + "\n";
  }
}
