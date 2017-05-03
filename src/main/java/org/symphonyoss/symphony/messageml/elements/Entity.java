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

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

/**
 * @author lukasz
 * @since 4/19/17
 */
public abstract class Entity extends Element {
  public static final String PRESENTATIONML_CLASS = "entity";
  public static final String ENTITY_ID_ATTR = "data-entity-id";
  static final String PRESENTATIONML_TAG = "span";
  public static final String TYPE_FIELD = "type";
  private static final String VERSION_FIELD = "version";
  public static final String VALUE_FIELD = "value";
  public static final String ID_FIELD = "id";

  String entityId;

  Entity(int index, Element parent, String messageMLTag, FormatEnum format) {
    super(index, parent, messageMLTag, format);
  }

  @Override
  public ObjectNode asEntityJson() {
    //Generate JSON only if we don't have a corresponding entity in EntityJson
    if (entityId == null) {
      ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
      node.put(TYPE_FIELD, getEntityType());
      node.put(VERSION_FIELD, getEntityVersion());

      ArrayNode idArray = new ArrayNode(JsonNodeFactory.instance);
      ObjectNode idNode = new ObjectNode(JsonNodeFactory.instance);
      idNode.put(TYPE_FIELD, getEntitySubType());
      idNode.put(VALUE_FIELD, getEntityValue());

      idArray.add(idNode);
      node.set(ID_FIELD, idArray);

      return node;
    }

    return null;
  }

  @Override
  public void validate() throws InvalidInputException {
    if (this.format == FormatEnum.PRESENTATIONML && this.entityId == null) {
      throw new InvalidInputException("The attribute \"data-entity-id\" is required");
    }
  }

  protected abstract String getEntityValue();

  protected abstract String getEntitySubType();

  protected abstract String getEntityVersion();

  protected abstract String getEntityType();
}
