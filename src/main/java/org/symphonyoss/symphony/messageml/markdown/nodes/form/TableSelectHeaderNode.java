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

package org.symphonyoss.symphony.messageml.markdown.nodes.form;

/**
 * Class representing a Markdown node for tableselect.
 *
 * @author Pedro Sanchez
 * @since 05/30/2019
 */
public class TableSelectHeaderNode extends FormElementNode {
  private final static String DELIMITER = " | ";

  private final static String LEFT = "left";
  private final static String RIGHT = "right";

  private final String position;
  private final String text;

  public TableSelectHeaderNode(String position, String text) {
    this.position = position;
    this.text = text;
  }

  public String getOpeningDelimiter() {
    if (LEFT.equals(position)) {
      return text + DELIMITER;
    }
    return "";
  }

  public String getClosingDelimiter() {
    if (RIGHT.equals(position)) {
      return DELIMITER + text + "\n";
    }
    return "\n";
  }
}
