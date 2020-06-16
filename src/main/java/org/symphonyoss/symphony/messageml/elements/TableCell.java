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
import org.symphonyoss.symphony.messageml.markdown.nodes.TableCellNode;

/**
 * Class representing a table cell container.
 *
 * @author lukasz
 * @since 3/27/17
 */
public class TableCell extends Element {
  public static final String MESSAGEML_TAG = "td";
  private static final String ATTR_ROWSPAN = "rowspan";
  private static final String ATTR_COLSPAN = "colspan";

  public TableCell(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ATTR_ROWSPAN:
        setAttribute(ATTR_ROWSPAN, getLongAttribute(item).toString());
        break;
      case ATTR_COLSPAN:
        setAttribute(ATTR_COLSPAN, getLongAttribute(item).toString());
        break;
      default:
        super.buildAttribute(parser, item);
    }
  }

  @Override
  public Node asMarkdown() {
    return new TableCellNode();
  }

  @Override
  public String toString() {
    return "Cell";
  }
}
