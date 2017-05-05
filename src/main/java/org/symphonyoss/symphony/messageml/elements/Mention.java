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
import org.commonmark.node.Text;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.MentionNode;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.IUserPresentation;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

/**
 * Class representing a convenience element for a user mention. Translated to an anchor element.
 * @author lukasz
 * @since 3/27/17
 */
public class Mention extends Entity {
  public static final String MESSAGEML_TAG = "mention";
  public static final String ENTITY_TYPE = "com.symphony.user.mention";

  static final String PREFIX = "@";
  private static final String ATTR_EMAIL = "email";
  private static final String ATTR_UID = "uid";
  private static final String ATTR_STRICT = "strict";
  private static final String ENTITY_SUBTYPE = "com.symphony.user.userId";
  private static final String ENTITY_VERSION = "1.0";

  private final IDataProvider dataProvider;

  private IUserPresentation userPresentation;
  private String email;
  private String prettyName;
  private Long uid;
  private boolean fallback = false;

  public Mention(int index, Element parent, IDataProvider dataProvider, FormatEnum format) {
    this(index, parent, null, false, dataProvider, format);
  }

  public Mention(int index, Element parent, Long uid, IDataProvider dataProvider, FormatEnum format) {
    this(index, parent, uid, false, dataProvider, format);
  }

  public Mention(int index, Element parent, Long uid, Boolean fallback, IDataProvider dataProvider, FormatEnum format) {
    super(index, parent, MESSAGEML_TAG, format);
    this.dataProvider = dataProvider;
    this.uid = uid;
    this.fallback = fallback;
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ENTITY_ID_ATTR:
        this.entityId = item.getTextContent();
        break;

      case ATTR_EMAIL:
        email = getStringAttribute(item);
        break;

      case ATTR_UID:
        uid = getLongAttribute(item);
        break;

      case ATTR_STRICT:
        fallback = !getBooleanAttribute(item);
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
  public void asPresentationML(XmlPrintStream out) {
    if (userPresentation != null) {
      String entityId = MESSAGEML_TAG + getIndex();
      out.printElement(PRESENTATIONML_TAG, asText(), CLASS_ATTR, PRESENTATIONML_CLASS, ENTITY_ID_ATTR, entityId);
    } else if (prettyName != null) {
      out.print(prettyName);
    } else if (email != null) {
      out.print(email);
    } else if (uid != null) {
      out.print(uid);
    }
  }

  @Override
  public Node asMarkdown() throws InvalidInputException {
    if (userPresentation == null) {
      if (prettyName != null) {
        return new Text(prettyName);
      } else if (email != null) {
        return new Text(email);
      } else if (uid != null) {
        return new Text(String.valueOf(uid));
      } else {
        throw new InvalidInputException("Error processing user mention. No id or email provided");
      }
    } else {
      return new MentionNode(userPresentation.getId(), userPresentation.getPrettyName(),
          userPresentation.getScreenName(), userPresentation.getEmail());
    }
  }

  @Override
  public ObjectNode asEntityJson() {
    if (userPresentation != null) {
      return super.asEntityJson();
    } else {
      return null;
    }
  }

  @Override
  public String asText() {
    return "@" + userPresentation.getPrettyName();
  }

  @Override
  public String toString() {
    String id;

    if (uid == null) {
      if (email == null) {
        id = "NULL";
      } else {
        id = email;
      }
    } else {
      id = String.valueOf(uid);
    }
    return "Mention(" + id + ")";
  }

  @Override
  public void validate() throws InvalidInputException {
    try {
      resolveUser();
    } catch (InvalidInputException e) {
      if (fallback) {
        userPresentation = null;
      } else {
        throw e;
      }
    }
  }

  private void resolveUser() throws InvalidInputException {
    if (uid != null) {
      userPresentation = dataProvider.getUserPresentation(uid);
    } else if (email != null) {
      userPresentation = dataProvider.getUserPresentation(email);
    }
  }

  public IUserPresentation getUserPresentation() {
    return userPresentation;
  }

  @Override
  public String getEntityId() {
    return String.format("%s%s", getMessageMLTag(), (entityId != null) ? entityId : getIndex());
  }

  @Override
  protected String getEntityValue() {
    return String.valueOf(userPresentation.getId());
  }

  @Override
  protected String getEntitySubType() {
    return ENTITY_SUBTYPE;
  }

  @Override
  protected String getEntityVersion() {
    return ENTITY_VERSION;
  }

  @Override
  protected String getEntityType() {
    return ENTITY_TYPE;
  }

}