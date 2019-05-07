package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.markdown.nodes.FormNode;

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
     This class was added here so the code for the Button class (APP-2058) could be complete.
  */
}
