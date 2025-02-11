package org.finos.symphony.messageml.messagemlutils.markdown.nodes.form;

public class TimePickerNode extends FormElementNode implements PlaceholderLabelTooltipNode {
  private final static String MARKDOWN = "Time Picker";

  private String label;
  private String tooltip;
  private String placeholder;


  public TimePickerNode(String label, String tooltip, String placeholder) {
    super(MARKDOWN, placeholder);
    this.label = label;
    this.tooltip = tooltip;
    this.placeholder = placeholder;
  }

  @Override
  public String getText() {
    return generateMarkdownPlaceholderLabelAndTooltip(placeholder, label, tooltip);
  }
}
