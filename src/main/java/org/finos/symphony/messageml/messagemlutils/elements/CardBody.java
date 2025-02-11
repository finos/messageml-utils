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

import org.commonmark.node.Node;
import org.commonmark.node.Paragraph;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.util.XmlPrintStream;

import java.util.Collections;

/**
 * Class representing a convenience element for a card body container. Translated to a div element.
 * @author lukasz
 * @since 3/27/17
 */
public class CardBody extends Element {

  public static final String MESSAGEML_TAG = "body";
  public static final String PRESENTATIONML_CLASS = "cardBody";
  private static final String PRESENTATIONML_TAG = "div";

  public CardBody(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
  }

  @Override
  void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    throwInvalidInputException(item);
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    out.openElement(PRESENTATIONML_TAG, Collections.singletonMap(CLASS_ATTR, PRESENTATIONML_CLASS));

    for (Element child : getChildren()) {
      child.asPresentationML(out, context);
    }

    out.closeElement();
  }

  @Override
  public Node asMarkdown() {
    return new Paragraph();
  }

  @Override
  void validate() throws InvalidInputException {
    assertNoAttributes();
    assertParent(Collections.singleton(Card.class));
  }

  @Override
  public String getPresentationMLTag() {
    return PRESENTATIONML_TAG;
  }
}
