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
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

/**
 * Class representing a horizontal rule.
 *
 * @author lukasz
 * @since 3/27/17
 */
public class HorizontalRule extends Element {

  public static final String MESSAGEML_TAG = "hr";
  private static final String MARKDOWN = "---";

  public HorizontalRule(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    throwInvalidInputException(item);
  }

  @Override
  public String asText() {
    return "\n";
  }

  @Override
  public Node asMarkdown() {
    org.commonmark.node.Paragraph node = new org.commonmark.node.Paragraph();
    node.appendChild(new org.commonmark.node.Text(MARKDOWN));
    return node;
  }

  @Override
  public void validate() throws InvalidInputException {
    assertNoContent();
    assertNoAttributes();
  }

  @Override
  public boolean areNestedElementsAllowed(){
    return false;
  }

  @Override
  public String toString() {
    return "\n";
  }

}
