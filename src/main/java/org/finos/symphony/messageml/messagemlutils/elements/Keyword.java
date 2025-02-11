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

package org.finos.symphony.messageml.messagemlutils.elements;

import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.util.XmlPrintStream;
import org.w3c.dom.Node;

/**
 * Base class of convenience elements for hash and cash tags.
 *
 * @author lukasz
 * @since 3/27/17
 */
abstract class Keyword extends Entity {

  private static final String ATTR_TAG = "tag";
  private static final String ENTITY_ID_PREFIX = "keyword";

  protected String tag;

  Keyword(Element parent, String messageMLTag, String presentationMlTag, FormatEnum format) {
    super(parent, messageMLTag, presentationMlTag, format);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
    if (ATTR_TAG.equals(item.getNodeName())) {
      this.tag = item.getTextContent();
    } else {
      super.buildAttribute(parser, item);
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
  protected String getEntityIdPrefix() {
    return ENTITY_ID_PREFIX;
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    out.printElement(presentationMLTag, asText(), CLASS_ATTR, Entity.PRESENTATIONML_CLASS, ENTITY_ID_ATTR, entityId);
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
