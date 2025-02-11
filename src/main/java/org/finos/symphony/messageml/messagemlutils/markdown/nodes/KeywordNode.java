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

import org.commonmark.node.CustomNode;

/**
 * Class representing a Markdown node for hash and cash tags.
 *
 * @author lukasz
 * @since 3/30/17
 */
public class KeywordNode extends CustomNode {
  private final String prefix;
  private final String text;

  public KeywordNode(String prefix, String text) {
    this.text = text;
    this.prefix = prefix;
  }

  public String getText() {
    return text;
  }

  public String getPrefix() {
    return prefix;
  }
}
