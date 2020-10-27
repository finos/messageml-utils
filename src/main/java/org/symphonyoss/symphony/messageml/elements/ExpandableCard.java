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

import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Node;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Class representing a convenience element which has a number of visual elements and which can be closed, expanded, cropped.
 * Translated to a div element.
 *
 * @author enrico.molino
 * @since 8/7/20
 */
public class ExpandableCard extends Element {

  public static final String MESSAGEML_TAG = "expandable-card";
  public static final String PRESENTATIONML_CLASS = "expandable-card";
  private static final String PRESENTATIONML_TAG = "div";
  private static final String ATTR_STATE = "state";
  private static final String PRESENTATIONML_STATE = "data-state";
  private static final List<String> allowedStates = Arrays.asList("collapsed", "cropped", "expanded");

  public ExpandableCard(Element parent, FormatEnum format) {
    super(parent, MESSAGEML_TAG, format);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case PRESENTATIONML_STATE:
      case ATTR_STATE:
        setAttribute(ATTR_STATE, getStringAttribute(item));
        break;
      default:
        super.buildAttribute(parser, item);
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    Map<String, String> presentationAttrs = new LinkedHashMap<>();
    if (getAttribute(CLASS_ATTR) != null) {
      presentationAttrs.put(CLASS_ATTR, String.format("%s %s", PRESENTATIONML_CLASS, getAttribute(CLASS_ATTR)));
    } else {
      presentationAttrs.put(CLASS_ATTR, PRESENTATIONML_CLASS);
    }
    if (getAttribute(ATTR_STATE) != null) {
      presentationAttrs.put(PRESENTATIONML_STATE, getAttribute(ATTR_STATE));
    }

    out.openElement(PRESENTATIONML_TAG, presentationAttrs);

    for (Element child : getChildren()) {
      child.asPresentationML(out, context);
    }

    out.closeElement();
  }

  @Override
  public String getPresentationMLTag() {
    return PRESENTATIONML_TAG;
  }

  @Override
  void validate() throws InvalidInputException {
    super.validate();

    assertAttributeNotBlank(ATTR_STATE);
    assertAttributeValue(ATTR_STATE, allowedStates);
  }
}
