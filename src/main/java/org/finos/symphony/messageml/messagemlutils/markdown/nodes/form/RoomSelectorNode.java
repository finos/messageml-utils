package org.finos.symphony.messageml.messagemlutils.markdown.nodes.form;


/**
 * Class that Represents a Markdown Node for the "RoomSelector" form element.
 *
 * @author Mohamed Rojbeni
 * @since 09/15/2023
 */
public class RoomSelectorNode extends FormElementNode implements PlaceholderLabelTooltipNode {
  private final static String MARKDOWN = "Room Selector";

  private String placeholder;
  private String label;
  private String tooltip;

  public RoomSelectorNode(String placeholder, String label, String tooltip) {
    super(MARKDOWN, placeholder);
    this.placeholder = placeholder;
    this.label = label;
    this.tooltip = tooltip;
  }

  @Override
  public String getText() {
    return generateMarkdownPlaceholderLabelAndTooltip(placeholder, label, tooltip);
  }
}
