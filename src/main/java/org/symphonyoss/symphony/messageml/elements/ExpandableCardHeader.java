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

import org.commonmark.node.Node;
import org.commonmark.node.Paragraph;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.util.Collections;

/**
 * Class representing a convenience element for an expandable card header container. Translated to a div element.
 *
 * @author enrico.molino
 * @since 8/7/20
 */
public class ExpandableCardHeader extends Element {

  public static final String MESSAGEML_TAG = "header";
  public static final String PRESENTATIONML_CLASS = "expandableCardHeader";
  private static final String PRESENTATIONML_TAG = "div";

  public ExpandableCardHeader(Element parent, FormatEnum format) {
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
    assertParent(Collections.singleton(ExpandableCard.class));
    assertNoAttributes();
  }

  @Override
  public String getPresentationMLTag() {
    return PRESENTATIONML_TAG;
  }
}
