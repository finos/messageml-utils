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
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.RadioNode;
import org.w3c.dom.Node;

import java.util.Arrays;
import java.util.Optional;

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
}
