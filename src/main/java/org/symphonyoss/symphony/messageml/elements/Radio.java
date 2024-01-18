/*
 * Copyright 2016-2017 MessageML - Symphony LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.RadioNode;
import org.w3c.dom.Node;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * Class representing Radio Buttons - Symphony Elements.
 *
 * @author Pedro Sanchez
 * @since 06/13/19
 */
public class Radio extends GroupedElement implements LabelableElement{

  public static final String MESSAGEML_TAG = "radio";

  public static final String PRESENTATIONML_INPUT_TYPE = "radio";
  public static final String PRESENTATIONML_DIV_CLASS = "radio-group";

  private static final String NAME_ATTR = "name";

  private static final String MARKDOWN = "Radio Button";

  public Radio(Element parent, FormatEnum messageFormat) {
    super(parent, MESSAGEML_TAG, messageFormat);
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    if (hasExactNumberOfChildren(1)) {
      return new RadioNode(getChildren().get(0).asText());
    }
    else {
      return new RadioNode();
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();
    assertAttributeNotBlank(NAME_ATTR);

    if(getAttribute(CHECKED_ATTR) != null) {
      assertAttributeValue(CHECKED_ATTR, Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    if (getAttribute(DISABLED_ATTR) != null) {
      assertAttributeValue(DISABLED_ATTR,
          Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    if (getAttribute(READONLY_ATTR) != null) {
      assertAttributeValue(READONLY_ATTR,
          Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    if (!getChildren().isEmpty()) {
      assertContentModel(Arrays.asList(TextNode.class, Bold.class, Italic.class));
    }
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case CHECKED_ATTR:
      case NAME_ATTR:
      case VALUE_ATTR:
      case LABEL:
      case DISABLED_ATTR:
      case READONLY_ATTR:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case ID_ATTR:
        if(this.format != FormatEnum.PRESENTATIONML){
          throwInvalidInputException(item);
        }
        fillAttributes(parser, item);
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  protected String getPresentationMLInputType() {
    return PRESENTATIONML_INPUT_TYPE;
  }

  @Override
  protected String getPresentationMLDivClass() {
    return PRESENTATIONML_DIV_CLASS;
  }

  @Override
  public String getElementId() {
    return MESSAGEML_TAG;
  }

  @Override
  public void updateBiContext(BiContext context) {
    Map<String, Object> attributesMapBi = new HashMap<>();

    attributesMapBi.put(BiFields.OPTIONS_COUNT.getValue(), 1);
    this.putOneIfPresent(attributesMapBi, BiFields.LABEL.getValue(), LABEL);
    this.computeAndPutDefault(context, attributesMapBi);

    context.updateItemCount(BiFields.RADIO.getValue(), attributesMapBi);
  }

  /**
   * This method will compute default property for this element : if {@link
   * GroupedElement#CHECKED_ATTR attribute is set to true}
   * It will update the context if and only if this current option is the first option
   * of the radio group to have default value set to true
   *
   * If {@link GroupedElement#CHECKED_ATTR} attribute is not explicitly set to true,
   * this default property will be considered as not set like in following :
   * <pre><radio checked=\"false\">Check Me if you can!</radio></pre>
   * <pre><radio checked=\"somethingElse\">Check Me if you can!</radio></pre>
   */
  private void computeAndPutDefault(BiContext context, Map<String, Object> attributesMapBi) {
    String isChecked = getAttribute(CHECKED_ATTR);
    boolean isDefaultAlreadySet =
        context.isAttributeSet(BiFields.RADIO.getValue(), BiFields.DEFAULT.getValue());
    if (isChecked != null && Boolean.TRUE.equals(Boolean.valueOf(isChecked)) && !isDefaultAlreadySet) {
      attributesMapBi.put(BiFields.DEFAULT.getValue(), 1);
    }
  }

}
