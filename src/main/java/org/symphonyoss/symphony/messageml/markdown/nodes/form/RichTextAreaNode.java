package org.symphonyoss.symphony.messageml.markdown.nodes.form;

public class RichTextAreaNode extends TextAreaNode{
  private static String MARKDOWN = "Rich Text Area";

  public RichTextAreaNode(String placeholder, String initialValue, String label, String tooltip) {
    super(MARKDOWN, placeholder, initialValue, label, tooltip);
  }
}
