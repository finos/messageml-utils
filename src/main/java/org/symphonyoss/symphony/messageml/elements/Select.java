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
import org.symphonyoss.symphony.messageml.markdown.nodes.form.SelectNode;
import org.w3c.dom.Node;

import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

/**
 * Class representing dropdown menu - Symphony Elements.
 *
 * @author lumoura
 * @since 3/22/18
 */
public class Select extends FormElement implements LabelableElement, TooltipableElement {

  public static final String MESSAGEML_TAG = "select";
  public static final String ELEMENT_ID = "dropdown";
  private static final String REQUIRED_ATTR = "required";
  private static final String OPTION_SELECTED_ATTR = "selected";
  private static final String DATA_PLACEHOLDER_ATTR = "data-placeholder";

  public Select(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new SelectNode(getAttribute(DATA_PLACEHOLDER_ATTR), getAttribute(LABEL), getAttribute(TITLE));
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    if (getAttribute(NAME_ATTR) == null) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }

    assertContentModel(Collections.singleton(Option.class));
    assertContainsChildOfType(Collections.singleton(Option.class));

    if(getAttribute(REQUIRED_ATTR) != null) {
      assertAttributeValue(REQUIRED_ATTR, Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    assertOnlyOneOptionSelected();
    assertAttributeNotBlank(NAME_ATTR);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case REQUIRED_ATTR:
      case DATA_PLACEHOLDER_ATTR:
      case LABEL:
      case TITLE:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case ID_ATTR:
        if(format != FormatEnum.PRESENTATIONML){
          throwInvalidInputException(item);
        }
        fillAttributes(parser, item);
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public String getElementId(){
    return ELEMENT_ID;
  }

  private void assertOnlyOneOptionSelected() throws InvalidInputException {
    long numberOfSelectedOptions = getChildren().stream()
        .map(child -> child.getAttribute(OPTION_SELECTED_ATTR))
        .filter(selectedAttr -> selectedAttr != null && selectedAttr.equalsIgnoreCase(Boolean.TRUE.toString()))
        .count();

    if(numberOfSelectedOptions > 1) {
      throw new InvalidInputException("Element \"select\" can only have one selected \"option\"");
    }
  }
}
