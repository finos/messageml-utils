package org.symphonyoss.symphony.messageml.elements;

import static java.lang.String.format;
import static org.symphonyoss.symphony.messageml.elements.Button.ACTION_TYPE;
import static org.symphonyoss.symphony.messageml.elements.FormElement.TYPE_ATTR;

import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.FormNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Node;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Class representing a Symphony Elements form
 *
 * @author lumoura
 * @since 03/21/19
 */
public class Form extends Element {
  public static final String MESSAGEML_TAG = "form";

  private static final String ID_ATTR = "id";
  private static final String MULTI_SUBMIT = "multi-submit";
  private static final String PRESENTATIONML_MULTI_SUBMIT = "data-multi-submit";
  private static final int MAX_COUNT_PER_CHILD_TYPE = 50;
  private static final int MAX_LENGTH = 64;
  private static final String ERR_MSG_MISSING_ACTION_BTN = "The form with id '%s' should have at least one action button";
  private static final List<String> ALLOWED_MULTI_SUBMIT = Arrays.asList("reset", "no-reset");

  public Form(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
  }

  @Override
  public Boolean hasIdAttribute() {
    return true;
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();
    assertNotParentAtAnyLevel(Collections.singletonList(this.getClass()));
    assertChildrenNotExceedingMaxCount(Arrays.asList(Checkbox.class, Radio.class), MAX_COUNT_PER_CHILD_TYPE);

    assertAttributeNotBlank(ID_ATTR);
    if (!getParent().getClass().equals(Dialog.class)) {
      assertAtLeastOneActionButton();
    } else {
      assertContentModel(Arrays.asList(DialogChild.Footer.class, DialogChild.Title.class, DialogChild.Body.class));
    }
    if(getAttribute(MULTI_SUBMIT) != null) {
      assertAttributeMaxLength(MULTI_SUBMIT, MAX_LENGTH);
      assertAttributeValue(MULTI_SUBMIT, ALLOWED_MULTI_SUBMIT);
    }
  }

  @Override
  protected void buildAttribute(MessageMLParser parser, Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ID_ATTR:
      case MULTI_SUBMIT:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new FormNode();
  }

  private void assertAtLeastOneActionButton() throws InvalidInputException {
    boolean hasActionButton = findElements(Button.class).stream()
        .anyMatch(element -> ACTION_TYPE.equals(element.getAttribute(TYPE_ATTR)));

    if (!hasActionButton) {
      throw new InvalidInputException(format(ERR_MSG_MISSING_ACTION_BTN, getAttribute(ID_ATTR)));
    }
  }

  @Override
  void updateBiContext(BiContext context) {
    Map<String, Object> formMap = new HashMap<>();
    if(getAttribute(MULTI_SUBMIT) != null){
      formMap.put(BiFields.MULTI_SUBMIT.getValue(), 1);
    }
    context.addItem(new BiItem(BiFields.FORM.getValue(), formMap));
  }

  @Override
  void asPresentationML(XmlPrintStream out, MessageMLContext context) {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();
    if (getAttribute(ID_ATTR) != null) {
      presentationAttrs.put(ID_ATTR, getAttribute(ID_ATTR));
    }
    if (getAttribute(MULTI_SUBMIT) != null) {
      presentationAttrs.put(PRESENTATIONML_MULTI_SUBMIT, getAttribute(MULTI_SUBMIT));
    }
    out.openElement(getPresentationMLTag(), presentationAttrs);
    for (Element child : getChildren()) {
      child.asPresentationML(out, context);
    }
    out.closeElement();
  }
}
