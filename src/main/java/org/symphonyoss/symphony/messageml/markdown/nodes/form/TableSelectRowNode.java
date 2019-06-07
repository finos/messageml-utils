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
public class TableSelectRowNode extends FormElementNode {
  private final static String DELIMITER = " | ";

  private final static String CHECKBOX_TAG = "(Checkbox)";

  private final static String BUTTON_TAG = "(Button:#)";

  private final static String LEFT = "left";
  private final static String RIGHT = "right";

  private final static String BUTTON = "button";
  private final static String CHECKBOX = "checkbox";

  private final String position;
  private final String tag;

  public TableSelectRowNode(String position, String type, String text) {
    this.position = position;

    if (BUTTON.equals(type)) {
      this.tag = BUTTON_TAG.replace("#", text);
    } else {
      this.tag = CHECKBOX_TAG;
    }
  }

  public String getOpeningDelimiter() {
    if (LEFT.equals(position)) {
      return tag + DELIMITER;
    }
    return "";
  }

  public String getClosingDelimiter() {
    if (RIGHT.equals(position)) {
      return DELIMITER + tag + "\n";
    }
    return "\n";
  }
}
