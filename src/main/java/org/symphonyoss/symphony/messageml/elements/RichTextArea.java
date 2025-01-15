package org.symphonyoss.symphony.messageml.elements;

import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.RichTextAreaNode;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.TextAreaNode;

public class RichTextArea extends TextArea {
  public static final String MESSAGEML_TAG = "richtextarea";
  public static final String PRESENTATIONML_TAG = "textarea";

  private static final String RICHTEXTAREA_ATTRIBUTE = "data-richtext";


  public RichTextArea(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    super.buildAttribute(parser, item);
    setAttribute(RICHTEXTAREA_ATTRIBUTE, "true");
  }


  @Override
  public String getElementId(){
    return MESSAGEML_TAG;
  }

  @Override
  public String getElementType() {
    return MESSAGEML_TAG;
  }

  @Override
  public String getPresentationMLTag() {
    return PRESENTATIONML_TAG;
  }

  @Override
  public void updateBiContext(BiContext context) {
    context.addItem(new BiItem(BiFields.RICH_TEXT_AREA.getValue(), computeCommonBiContext()));
  }

  @Override
  public Node asMarkdown() {
    return new RichTextAreaNode(getAttribute(PLACEHOLDER_ATTR), hasExactNumberOfChildren(1) ? getChild(0).asText() : null,
        getAttribute(LABEL), getAttribute(TITLE));
  }


}
