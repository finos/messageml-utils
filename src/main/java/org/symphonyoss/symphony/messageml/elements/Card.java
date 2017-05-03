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

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Class representing a convenience element which has a number of visual elements and which can be opened and closed.
 * Translated to a div element.
 *
 * @author lukasz
 * @since 3/27/17
 */
public class Card extends Element {

  public static final String MESSAGEML_TAG = "card";
  public static final String PRESENTATIONML_CLASS = "card";
  private static final String PRESENTATIONML_TAG = "div";
  private static final String ATTR_ICON = "iconSrc";
  private static final String PRESENTATIONML_ICON = "data-icon-src";

  public Card(int index, Element parent, FormatEnum format) {
    super(index, parent, MESSAGEML_TAG, format);
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ATTR_ICON:
        setAttribute(ATTR_ICON, getStringAttribute(item));
        break;
      default:
        super.buildAttribute(item);
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();
    if (getAttribute(CLASS_ATTR) != null) {
      presentationAttrs.put(CLASS_ATTR, String.format("%s %s", PRESENTATIONML_CLASS, getAttribute(CLASS_ATTR)));
    } else {
      presentationAttrs.put(CLASS_ATTR, PRESENTATIONML_CLASS);
    }
    if (getAttribute(ATTR_ICON) != null) {
      presentationAttrs.put(PRESENTATIONML_ICON, getAttribute(ATTR_ICON));
    }

    out.openElement(PRESENTATIONML_TAG, presentationAttrs);

    for (Element child : getChildren()) {
      child.asPresentationML(out);
    }

    out.closeElement();
  }

}
