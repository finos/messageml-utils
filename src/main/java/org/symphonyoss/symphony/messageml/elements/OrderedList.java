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
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

/**
 * Class representing an ordered list container.
 *
 * @author lukasz
 * @since 3/27/17
 */
public class OrderedList extends Element {

  public static final String MESSAGEML_TAG = "ol";

  public OrderedList(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public Node asMarkdown() {
    org.commonmark.node.OrderedList ol = new org.commonmark.node.OrderedList();
    ol.setStartNumber(1);
    ol.setDelimiter('.');
    return ol;
  }

  @Override
  public void validate() throws InvalidInputException {
    assertNoText();
    assertContentModel(Collections.<Class<? extends Element>>singleton(ListItem.class));
    assertContainsChildOfType(Collections.<Class<? extends Element>>singleton(ListItem.class));
  }

  @Override
  void updateBiContext(BiContext context) {
    super.updateBiContext(context);
    context.updateItem("Lists");
  }
}
