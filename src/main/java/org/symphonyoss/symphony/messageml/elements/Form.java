package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormNode;

/**
 * Class representing a Symphony Elements form
 *
 * @author lumoura
 * @since 03/21/19
 */
public class Form extends Element {
  public static final String MESSAGEML_TAG = "form";

  public Form(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new FormNode();
  }

  /* TODO:  The code for this class is supposed to be implemented in task APP-2055.
     This class was added here so the code for the Select class (APP-2056) could be complete.
  */
}