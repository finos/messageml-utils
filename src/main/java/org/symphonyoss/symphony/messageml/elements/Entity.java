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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.w3c.dom.Node;

/**
 * @author lukasz
 * @since 4/19/17
 */
public abstract class Entity extends Element {
  public static final String PRESENTATIONML_CLASS = "entity";
  public static final String ENTITY_ID_ATTR = "data-entity-id";
  public static final String TYPE_FIELD = "type";
  public static final String VALUE_FIELD = "value";
  public static final String ID_FIELD = "id";
  static final String DEFAULT_PRESENTATIONML_TAG = "span";
  static final String VERSION_FIELD = "version";
  String presentationMLTag;

  String entityId;

  Entity(Element parent, String messageMLTag, String presentationMlTag, FormatEnum format) {
    super(parent, messageMLTag, format);
    this.presentationMLTag = presentationMlTag;
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ENTITY_ID_ATTR:
        this.entityId = item.getTextContent();
        break;
      default:
        if (format == FormatEnum.PRESENTATIONML) {
          super.buildAttribute(parser, item);
        } else {
          throwInvalidInputException(item);
        }
    }
  }

  @Override
  public ObjectNode asEntityJson(ObjectNode parent) {
    JsonNode entityNode = parent.path(entityId);

    //Generate JSON only if we don't have a corresponding entity in EntityJson
    if (entityNode.isMissingNode()) {
      ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
      node.put(TYPE_FIELD, getEntityType());
      node.put(VERSION_FIELD, getEntityVersion());

      ArrayNode idArray = new ArrayNode(JsonNodeFactory.instance);
      ObjectNode idNode = new ObjectNode(JsonNodeFactory.instance);
      idNode.put(TYPE_FIELD, getEntitySubType());
      idNode.put(VALUE_FIELD, getEntityValue());

      idArray.add(idNode);
      node.set(ID_FIELD, idArray);

      parent.set(entityId, node);
      return node;
    } else {
      //For preexisting data-entity-id the node type has already been validated by MessageMLParser
      return (ObjectNode) entityNode;
    }

  }

  @Override
  public void validate() throws InvalidInputException {
    if (this.format == FormatEnum.PRESENTATIONML && this.entityId == null) {
      throw new InvalidInputException("The attribute \"data-entity-id\" is required");
    }
  }

  String getEntityId(int index) {
    return String.format("%s%s", getEntityIdPrefix(), index);
  }

  @Override
  public String getPresentationMLTag() {
    return presentationMLTag;
  }

  protected abstract String getEntityValue();

  protected abstract String getEntitySubType();

  protected abstract String getEntityVersion();

  protected abstract String getEntityType();

  protected abstract String getEntityIdPrefix();
}
