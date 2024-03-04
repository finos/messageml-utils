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

package org.symphonyoss.symphony.messageml.markdown.nodes;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.Getter;
import org.commonmark.node.CustomNode;
import org.symphonyoss.symphony.messageml.util.instrument.resolver.Instrument;

/**
 * Class representing a Markdown node for financial tags.
 *
 */
public class TagNode extends CustomNode {

  @Getter
  private String prefix;
  @Getter
  private String text;
  @Getter
  private JsonNode data;

  public TagNode(String prefix, String text, JsonNode data) {
    this.prefix = prefix;
    this.text = text;
    this.data = data;
  }

}

