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
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

/**
 * Base class of convenience elements for hash and cash tags.
 *
 * @author lukasz
 * @since 3/27/17
 */
abstract class Keyword extends Entity {

  private static final String ATTR_TAG = "tag";
  private static final String ENTITY_ID_PREFIX = "keyword";

  private String tag;

  Keyword(int index, Element parent, String messageMlTag, String tag, FormatEnum format) {
    super(index, parent, messageMlTag, format);
    this.tag = tag;
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ENTITY_ID_ATTR:
        this.entityId = item.getTextContent();
        break;
      case ATTR_TAG:
        this.tag = item.getTextContent();
        break;
      default:
        if (format == FormatEnum.PRESENTATIONML) {
          super.buildAttribute(item);
        } else {
          throw new InvalidInputException("Attribute \"" + item.getNodeName()
              + "\" is not allowed in \"" + getMessageMLTag() + "\"");
        }
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    if (this.tag == null) {
      throw new InvalidInputException("The attribute \"tag\" is required");
    }

    super.validate();
  }

  public String getTag() {
    return this.tag;
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    String entityId = ENTITY_ID_PREFIX + getIndex();
    out.printElement(PRESENTATIONML_TAG, asText(), CLASS_ATTR, Entity.PRESENTATIONML_CLASS, ENTITY_ID_ATTR, entityId);
  }

  @Override
  public String getEntityId() {
    return String.format("%s%s", ENTITY_ID_PREFIX, (entityId != null) ? entityId : getIndex());
  }

  @Override
  protected String getEntityValue() {
    return getTag();
  }

  @Override
  public String toString() {
    return "Keyword(" + getTag() + ")";
  }
}
