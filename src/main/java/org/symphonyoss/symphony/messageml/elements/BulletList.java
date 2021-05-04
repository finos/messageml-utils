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

import java.util.Collections;

/**
 * Class representing a bulleted list container.
 *
 * @author lukasz
 * @since 3/27/16
 */
public class BulletList extends Element {
  public static final String MESSAGEML_TAG = "ul";

  public BulletList(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public Node asMarkdown() {
    org.commonmark.node.BulletList ul = new org.commonmark.node.BulletList();
    ul.setBulletMarker('-');
    ul.setTight(false);
    return ul;
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
    context.updateItemCount(BiFields.BULLET_LIST.getFieldName());
  }
}
