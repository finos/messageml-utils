/* ----------------------------------------------------------------------------
 * Copyright (C) 2016
 * Symphony Communication Services, LLC
 * All Rights Reserved
 * ---------------------------------------------------------------------------- */
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


import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.commonmark.node.Document;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Node;


/**
 * Class representing a MessageML document (i.e. a message).
 *
 * A MessageML document tree is constructed through one of parse() methods in
 * {@link org.symphonyoss.symphony.messageml.MessageMLContext}.
 * @author lukasz
 * @since 3/27/17
 */
public class MessageML extends Element {

  public static final String MESSAGEML_VERSION = "2.0";
  public static final String MESSAGEML_TAG = "messageML";
  public static final String PRESENTATIONML_TAG = "div";
  private static final String ATTR_FORMAT = "data-format";
  private static final String ATTR_VERSION = "data-version";
  private static final String PRESENTATIONML_FORMAT = "PresentationML";

  private String version;
  private boolean chime;

  public MessageML(FormatEnum format, String version) {
    super(null, MESSAGEML_TAG, format);
    this.version = version;
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
    if (getFormat() == FormatEnum.PRESENTATIONML) {
      switch (item.getNodeName()) {
        case ATTR_FORMAT:
          setAttribute(ATTR_FORMAT, getStringAttribute(item));
          break;

        case ATTR_VERSION:
          this.version = getStringAttribute(item);
          break;

        default:
          super.buildAttribute(parser, item);
      }
    } else {
      super.buildAttribute(parser, item);
    }
  }

  @Override
  public Document asMarkdown() throws InvalidInputException {
    Document root = new Document();
    try {
      buildMarkdown(root);
    } catch (IllegalArgumentException e) {
      throw new InvalidInputException("Failed to build Markdown: " + e.getMessage());
    }
    return root;
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {

    out.openElement(PRESENTATIONML_TAG, ATTR_FORMAT, PRESENTATIONML_FORMAT, ATTR_VERSION, version);

    for (Element child : getChildren()) {
      child.asPresentationML(out, context);
    }

    out.closeElement();
  }

  @Override
  public ObjectNode asEntityJson(ObjectNode parent) {
    if (parent == null) {
      parent = new ObjectNode(JsonNodeFactory.instance);
    }

    buildEntityJson(parent);
    return parent;
  }

  @Override
  public void validate() throws InvalidInputException {
    if (format == FormatEnum.MESSAGEML) {

      assertNoAttributes();

    } else if (format == FormatEnum.PRESENTATIONML) {

      if (!PRESENTATIONML_FORMAT.equalsIgnoreCase(getAttribute(ATTR_FORMAT)) || this.version == null) {
        throw new InvalidInputException("Malformed PresentationML. The attributes \"" + ATTR_FORMAT
            + "\" and \"" + ATTR_VERSION + "\" are required.");
      }

    }

    if (isChime()) {

      if (getChildren().size() != 1 || !(getChild(0) instanceof Chime)) {
        throw new InvalidInputException("Chime messages may not have any other content");
      }

    }
  }

  /**
   * Return whether this message is a chime.
   */

  public boolean isChime() {
    return chime;
  }

  /**
   * Return whether this message is a chime.
   */
  public void setChime(boolean chime) {
    this.chime = chime;
  }

  @Override
  public String getPresentationMLTag() {
    return PRESENTATIONML_TAG;
  }
}
