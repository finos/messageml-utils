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

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.SelectNode;

import java.util.Arrays;
import java.util.Collections;

/**
 * Class representing dropdown menu - Symphony Elements.
 *
 * @author lumoura
 * @since 3/22/18
 */
public class Select extends FormElement {

  public static final String MESSAGEML_TAG = "select";
  private static final String REQUIRED_ATTR = "required";
  private static final String OPTION_SELECTED_ATTR = "selected";
  private static final String DATA_PLACEHOLDER_ATTR = "data-placeholder";


  public Select(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new SelectNode(getAttribute(NAME_ATTR));
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
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
        setAttribute(NAME_ATTR, getStringAttribute(item));
        break;
      case REQUIRED_ATTR:
        setAttribute(REQUIRED_ATTR, getStringAttribute(item));
        break;
      case DATA_PLACEHOLDER_ATTR:
        setAttribute(DATA_PLACEHOLDER_ATTR, getStringAttribute(item));
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
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
