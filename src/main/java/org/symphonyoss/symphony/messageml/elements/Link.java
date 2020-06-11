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

import org.apache.commons.lang3.StringUtils;
import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * Class representing a link.
 * @author lukasz
 * @since 3/27/17
 */
public class Link extends Element {
  public static final String MESSAGEML_TAG = "a";
  private static final String ATTR_HREF = "href";
  private final IDataProvider dataProvider;

  private URI uri;

  public Link(Element parent, IDataProvider dataProvider) throws InvalidInputException {
    this(parent, null, dataProvider);
  }

  public Link(Element parent, String href, IDataProvider dataProvider) throws InvalidInputException {
    super(parent, MESSAGEML_TAG);
    this.dataProvider = dataProvider;

    if (href != null) {
      try {
        setAttribute(ATTR_HREF, href);
        this.uri = new URI(href);
      } catch (URISyntaxException e) {
        throw new InvalidInputException("Error processing URI: " + e.getMessage());
      }
    }
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ATTR_HREF:
        uri = getUrlAttribute(item);
        setAttribute(ATTR_HREF, uri.toString());
        break;

      default:
        super.buildAttribute(parser, item);
    }
  }

  @Override
  void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    out.openElement(getMessageMLTag(), getAttributes());

    if (getChildren().isEmpty()) {
      out.print(out.escape(getUri().toString()));
    } else {
      for (Element child : getChildren()) {
        child.asPresentationML(out, context);
      }
    }

    out.closeElement();
  }

  @Override
  public Node asMarkdown() {
    return new org.commonmark.node.Link(getUri().toString(), asText());
  }

  @Override
  public void validate() throws InvalidInputException {

    if (uri == null) {
      throw new InvalidInputException("The attribute \"href\" is required");
    }

    if (StringUtils.isBlank(uri.toString())) {
      throw new InvalidInputException("The attribute \"href\" cannot be empty");
    }

    if (!uri.isAbsolute()) {
      throw new InvalidInputException("The attribute \"href\" must contain an absolute URI");
    }

    try {
      dataProvider.validateURI(uri);
    } catch (ProcessingException e) {
      throw new InvalidInputException(e.getMessage());
    }

  }

  public URI getUri() {
    return uri;
  }

  public void setUri(URI url) {
    uri = url;
  }

}
