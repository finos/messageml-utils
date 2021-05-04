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
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.TableRowNode;

import java.util.Arrays;

/**
 * Class representing a table row container.
 *
 * @author lukasz
 * @since 3/27/17
 */
public class TableRow extends Element {
  public static final String MESSAGEML_TAG = "tr";

  public TableRow(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public Node asMarkdown() {
    return new TableRowNode();
  }

  @Override
  public String toString() {
    return "Row";
  }

  @Override
  public void validate() throws InvalidInputException {
    assertNoText();
    assertContentModel(Arrays.asList(TableHeaderCell.class, TableCell.class));
  }

  @Override
  void updateBiContext(BiContext context) {
    super.updateBiContext(context);
    context.updateItemWithMaxValue(BiFields.TABLE_COLUMN_MAX.getFieldName(), countChildrenOfType(TableCell.class));
  }
}
