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
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.markdown.nodes.PreformattedNode;

import java.util.Arrays;

/**
 * Class representing preformatted text.
 * @author lukasz
 * @since 6/27/17
 */
public class Preformatted extends Element {
  public static final String MESSAGEML_TAG = "pre";

  public Preformatted(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public void buildAll(MessageMLParser context, org.w3c.dom.Element element) throws InvalidInputException,
      ProcessingException {
    boolean nl = TextNode.isRemoveNewLines();
    TextNode.setRemoveNewLines(false);
    super.buildAll(context, element);
    TextNode.setRemoveNewLines(nl);
  }

  @Override
  public Node asMarkdown() {
    return new PreformattedNode();
  }

  @Override
  public void validate() throws InvalidInputException {
    assertContentModel(Arrays.asList(TextNode.class, Link.class, Chime.class, Bold.class, Italic.class, Image.class, LineBreak
            .class,
        Span.class));
  }
}
