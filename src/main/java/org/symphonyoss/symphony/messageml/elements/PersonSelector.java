package org.symphonyoss.symphony.messageml.elements;

import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormElementNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

/**
 * Class representing a person selector inside a Symphony Elements form.
 *
 * @author Cristiano Faustino
 * @since 06/11/2019
 */
public class PersonSelector extends FormElement {
  public static final String MESSAGEML_TAG = "person-selector";

  private static final String PRESENTATIONML_TAG = "div";
  private final static String MARKDOWN = "Person Selector";
  private static final String CLASS_ATTR = "class";

  public PersonSelector(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();
    assertNoAttributes();
    assertNoContent();
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    out.printElement(PRESENTATIONML_TAG, null, CLASS_ATTR, MESSAGEML_TAG);
  }

  @Override
  public Node asMarkdown() {
    return new FormElementNode(MARKDOWN);
  }
}
