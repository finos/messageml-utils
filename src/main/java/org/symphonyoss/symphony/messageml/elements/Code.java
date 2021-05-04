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

import org.commonmark.node.FencedCodeBlock;
import org.commonmark.node.Node;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

/**
 * Class representing a block container for block or inline content.
 * @author lukasz
 * @since 3/27/17
 */
public class Code extends Element {

  public static final String MESSAGEML_TAG = "code";
  public static final char MARKDOWN_DELIMITER_CHAR = '`';
  public static final int MARKDOWN_DELIMITER_LENGTH = 3;
  private static final int MARKDOWN_DELIMITER_INDENT = 0;

  public Code(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public Node asMarkdown() {
    FencedCodeBlock node = new FencedCodeBlock();
    node.setFenceChar(MARKDOWN_DELIMITER_CHAR);
    node.setFenceLength(MARKDOWN_DELIMITER_LENGTH);
    node.setFenceIndent(MARKDOWN_DELIMITER_INDENT);
    return node;
  }

  @Override
  public void validate() throws InvalidInputException {
    assertNoAttributes();
    assertPhrasingContent();
  }

  @Override
  void updateBiContext(BiContext context) {
    super.updateBiContext(context);
    context.updateItemCount(BiFields.CODE.getFieldName());
  }
}
