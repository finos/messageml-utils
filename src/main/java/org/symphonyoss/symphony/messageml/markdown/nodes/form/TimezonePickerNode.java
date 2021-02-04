package org.symphonyoss.symphony.messageml.markdown.nodes.form;

public class TimezonePickerNode extends FormElementNode implements PlaceholderLabelTooltipNode {

  private final static String MARKDOWN = "Timezone Picker";

  private final String label;
  private final String tooltip;
  private final String placeholder;


  public TimezonePickerNode(String label, String tooltip, String placeholder) {
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
