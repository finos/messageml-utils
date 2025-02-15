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

import org.commonmark.node.Node;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.markdown.nodes.PreformattedNode;
import org.finos.symphony.messageml.messagemlutils.util.XmlPrintStream;

/**
 * Class representing preformatted text.
 * @author lukasz
 * @since 6/27/17
 */
public class Preformatted extends Element {
  public static final String MESSAGEML_TAG = "pre";

  public Preformatted(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    boolean removeNl = out.isRemoveNl();

    out.setRemoveNl(false);

    super.asPresentationML(out, context);

    out.setRemoveNl(removeNl);
  }

  @Override
  public Node asMarkdown() throws InvalidInputException {
    return new PreformattedNode();
  }

  @Override
  public void validate() throws InvalidInputException {
    assertPhrasingContent();
  }

  @Override
  void updateBiContext(BiContext context) {
    super.updateBiContext(context);
    context.updateItemCount(BiFields.PREFORMATTED.getValue());
  }
}
