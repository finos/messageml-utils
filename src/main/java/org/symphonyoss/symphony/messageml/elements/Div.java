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

import com.fasterxml.jackson.databind.node.ObjectNode;
import org.commonmark.node.Node;
import org.commonmark.node.Paragraph;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

/**
 * Class representing a block container for block or inline content.
 * @author lukasz
 * @since 3/27/17
 */
public class Div extends Element {

  public static final String MESSAGEML_TAG = "div";
  private static final String ATTR_ENTITY_ID = "data-entity-id";
  private static final String ATTR_ICON_SRC = "data-icon-src";
  private static final String ATTR_ACCENT_COLOR = "data-accent-color";

  public Div(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ATTR_ENTITY_ID:
        setAttribute(ATTR_ENTITY_ID, getStringAttribute(item));
        break;

      case ATTR_ICON_SRC:
        setAttribute(ATTR_ICON_SRC, getStringAttribute(item));
        break;

      case ATTR_ACCENT_COLOR:
        setAttribute(ATTR_ACCENT_COLOR, getStringAttribute(item));
        break;

      default:
        super.buildAttribute(item);
    }
  }

  @Override
  public Node asMarkdown() {
    return new Paragraph();
  }

  @Override
  public ObjectNode asEntityJson(ObjectNode parent) {
    String entityId = getAttribute(ATTR_ENTITY_ID);

    if (entityId != null) {
      //The existence and type of EntityJSON data has already been validated by MessageMLParser
      return (ObjectNode) parent.path(entityId);
    }

    return null;
  }

  @Override
  public void validate() throws InvalidInputException {
    String classAttr = getAttribute(CLASS_ATTR);

    if (getAttribute(ATTR_ENTITY_ID) != null && !"entity".equals(classAttr)) {
      throw new InvalidInputException("The attribute \"" + ATTR_ENTITY_ID + "\" is only allowed if the element "
          + "class is \"entity\".");
    }

    if ("entity".equals(classAttr) && getAttribute(ATTR_ENTITY_ID) == null) {
      throw new InvalidInputException("The attribute \"" + ATTR_ENTITY_ID + "\" is required if the element "
          + "class is \"entity\".");
    }

    String[] classes = (classAttr != null) ? classAttr.split(" ") : null;

    for (String attr : new String[] {ATTR_ICON_SRC, ATTR_ACCENT_COLOR}) {

      if (getAttribute(attr) != null) {
        if (classes == null || classes.length == 0 || !"card".equals(classes[0])) {
          throw new InvalidInputException("The attribute \"" + attr + "\" is only allowed if the element "
              + "class is \"card\".");
        }
      }
    }
  }
}
