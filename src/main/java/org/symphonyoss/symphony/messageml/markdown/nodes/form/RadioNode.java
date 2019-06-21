package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class implemented to have the spec for markdown parsing of Radio Buttons for Form Elements.
 *
 * @author Pedro Sanchez
 * @since 06/13/19
 */
public class RadioNode extends FormElementNode {
  private final static String MARKDOWN = "Radio Button:";

  public RadioNode() {
    super(MARKDOWN);
  }
}
