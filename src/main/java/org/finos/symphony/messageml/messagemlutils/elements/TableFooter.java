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

package org.finos.symphony.messageml.messagemlutils.elements;

import org.finos.symphony.messageml.messagemlutils.bi.BiContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

import java.util.Collections;

/**
 * Class representing a table footer container.
 *
 * @author lukasz
 * @since 3/27/17
 */
public class TableFooter extends Element {
  public static final String MESSAGEML_TAG = "tfoot";

  public TableFooter(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public String toString() {
    return "Footer";
  }

  @Override
  public void validate() throws InvalidInputException {
    assertNoText();
    assertContentModel(Collections.<Class<? extends Element>>singleton(TableRow.class));
  }

  @Override
  void updateBiContext(BiContext context) {
    super.updateBiContext(context);
    context.updateItemCount(BiFields.TABLE_FOOTER.getValue());
  }
}
