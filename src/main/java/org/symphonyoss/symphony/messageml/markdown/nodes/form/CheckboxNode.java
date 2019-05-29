package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class implemented to have the spec for markdown parsing of Checkboxes for Symphony Elements.
 * @author Cristiano Faustino
 * @since 05/28/2019
 */
public class CheckboxNode extends FormElementNode {
  private final static String TAG_REPRESENTATION_ON_MARKDOWN = "Checkbox:";

  private boolean hasText;

  public CheckboxNode(Boolean hasText, String name) {
    super(TAG_REPRESENTATION_ON_MARKDOWN, name);
    this.hasText = hasText;
  }

  public boolean hasText() {
    return hasText;
  }
}
