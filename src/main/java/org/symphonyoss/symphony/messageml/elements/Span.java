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
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.w3c.dom.Node;

/**
 * Class representing an inline container for inline content.
 *
 * @author lukasz
 * @since 3/27/17
 */
public class Span extends Element {

  public static final String MESSAGEML_TAG = "span";
  private static final String ATTR_ENTITY_ID = "data-entity-id";

  public Span(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
      switch (item.getNodeName()) {
        case ATTR_ENTITY_ID:
        // A span can be also generated to contains a tooltip
        case TooltipableElement.DATA_TITLE:
        case TooltipableElement.DATA_TARGET_ID:
          setAttribute(item.getNodeName(), getStringAttribute(item));
          break;
        default:
          super.buildAttribute(parser, item);
      }
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
    if (getAttribute(ATTR_ENTITY_ID) != null && !"entity".equals(getAttribute(CLASS_ATTR))) {
      throw new InvalidInputException("The attribute \"" + ATTR_ENTITY_ID + "\" is only allowed if the element "
          + "class is \"entity\".");
    }

    if ("entity".equals(getAttribute(CLASS_ATTR)) && getAttribute(ATTR_ENTITY_ID) == null)  {
      throw new InvalidInputException("The attribute \"" + ATTR_ENTITY_ID + "\" is required if the element "
          + "class is \"entity\".");
    }
  }
}
