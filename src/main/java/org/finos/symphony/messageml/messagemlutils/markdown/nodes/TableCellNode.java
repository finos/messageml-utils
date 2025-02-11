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

package org.finos.symphony.messageml.messagemlutils.markdown.nodes;

import org.commonmark.node.CustomBlock;

/**
 * Class representing a Markdown node for table cells.
 *
 * @author lukasz
 * @since 4/7/17
 */
public class TableCellNode extends CustomBlock {
  private final static String DELIMITER = "   ";

  public String getDelimiter() {
    return DELIMITER;
  }
}
