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
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
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
  private static final String MAILTO = "mailto:";

  private final IDataProvider dataProvider;

  private IUserPresentation userPresentation;
  private String email;
  private String prettyName;
  private Long uid;
  private boolean fallback = false;

  public Mention(Element parent, int entityIndex, IDataProvider dataProvider) {
    this(parent, DEFAULT_PRESENTATIONML_TAG, entityIndex, null, false, dataProvider,
        FormatEnum.MESSAGEML);
  }

  public Mention(Element parent, int entityIndex, Long uid, IDataProvider dataProvider) {
    this(parent, DEFAULT_PRESENTATIONML_TAG, entityIndex, uid, true, dataProvider,
        FormatEnum.MESSAGEML);
  }

  public Mention(Element parent, String presentationMlTag, Long uid, IDataProvider dataProvider) {
    this(parent, presentationMlTag, 0, uid, false, dataProvider, FormatEnum.PRESENTATIONML);
  }

  private Mention(Element parent, String presentationMlTag, Integer entityIndex, Long uid,
      Boolean fallback, IDataProvider dataProvider, FormatEnum format) {
    super(parent, MESSAGEML_TAG, presentationMlTag, format);
    this.dataProvider = dataProvider;
    this.uid = uid;
    this.fallback = fallback;
    this.entityId = getEntityId(entityIndex);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
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
        super.buildAttribute(parser, item);
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    if (userPresentation != null) {
      out.printElement(presentationMLTag, asText(), CLASS_ATTR, PRESENTATIONML_CLASS,
          ENTITY_ID_ATTR, entityId);
    } else {
      if (uid != null) {
        out.printElement(presentationMLTag, String.valueOf(uid), CLASS_ATTR, PRESENTATIONML_CLASS,
            ENTITY_ID_ATTR, entityId);
      } else if (prettyName != null) {
        out.print(prettyName);
      } else if (email != null) {
        try {
          Link link = new Link(getParent(), buildMailTo(), dataProvider);

          for (Element child : getChildren()) {
            link.addChild(child);
          }

          // If there's no pretty text, adds only the email as the tag text
          if(link.getChildren().isEmpty()) {
            TextNode child = new TextNode(link, email);
            link.addChild(child);
          }

          link.asPresentationML(out, context);
        } catch (InvalidInputException e) { // Thrown on unsupported protocol
          out.print(email);
        }
      }
    }
  }

  @Override
  public Node asMarkdown() throws InvalidInputException {
    if (userPresentation == null) {
      if (prettyName != null) {
        return new Text(prettyName);
      } else if (email != null) {
        return new Text((prettyName != null) ? prettyName : email);
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
  public ObjectNode asEntityJson(ObjectNode parent) {
    if (getEntityValue() != null) {
      return super.asEntityJson(parent);
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

    if (userPresentation != null) {
      uid = (uid == null) ? userPresentation.getId() : uid;
      email = (email == null) ? userPresentation.getEmail() : email;
      prettyName = (prettyName == null) ? userPresentation.getPrettyName() : prettyName;
    }
  }

  private String buildMailTo() {
    return MAILTO + email;
  }

  public IUserPresentation getUserPresentation() {
    return userPresentation;
  }

  @Override
  protected String getEntityIdPrefix() {
    return MESSAGEML_TAG;
  }

  @Override
  protected String getEntityValue() {
    return (uid != null) ? String.valueOf(uid) : null;
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
