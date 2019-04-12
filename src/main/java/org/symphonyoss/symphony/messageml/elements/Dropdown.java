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

import java.util.Collections;

/**
 * Class representing dropdown menu - Symphony Elements.
 *
 * @author lumoura
 * @since 3/22/18
 */
public class Dropdown extends Element {

  public static final String MESSAGEML_TAG = "dropdown";
  public static final String ID_ATTR = "id";
  public static final String REQUIRED_ATTR = "required";

  public Dropdown(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ID_ATTR:
        setAttribute(ID_ATTR, getStringAttribute(item));
        break;

      case REQUIRED_ATTR:
        String requiredAttrValue = getStringAttribute(item).toLowerCase();
        if (requiredAttrValue.equals("true")) {
          setAttribute(REQUIRED_ATTR, "true");
        } else if (!requiredAttrValue.equals("false")) {
          throw new InvalidInputException("Attribute \"" + REQUIRED_ATTR + "\" must be either \"true\" or \"false\"");
        }
        break;

      case CLASS_ATTR:
        setAttribute(CLASS_ATTR, getStringAttribute(item));
        break;

      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
                + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  private void assertAtLeastOneChild() throws InvalidInputException {
    if (this.getChildren().size() == 0) {
      throw new InvalidInputException("The \"" + getMessageMLTag() + "\" element must have at least one \""
              + Option.MESSAGEML_TAG + "\" as its child.");
    }
    if (this.getChildren().size() == 1 && this.getChildren().get(0).getClass() == TextNode.class) {
      throw new InvalidInputException("The \"" + getMessageMLTag() + "\" element must have at least one \""
              + Option.MESSAGEML_TAG + "\" as its child.");
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    if (this.getParent().getClass() != Form.class) {
      throw new InvalidInputException("A \"" + getMessageMLTag() + "\" element can only be a child of a \"" +
              "form\" element");
    }
    if (getAttribute(ID_ATTR) == null) {
      throw new InvalidInputException("The attribute \"id\" is required");
    }
    assertAtLeastOneChild();
    assertContentModel(Collections.<Class<? extends Element>>singleton(Option.class));
  }
}
