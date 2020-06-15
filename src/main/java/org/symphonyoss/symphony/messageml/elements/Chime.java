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

/**
 * Class representing a convenience element for a chime. Translated to an audio element.
 *
 * @author lukasz
 * @since 3/27/17
 */
public class Chime extends Element {
  public static final String MESSAGEML_TAG = "chime";
  public static final String PRESENTATIONML_TAG = "audio";
  private static final String ATTR_SRC = "src";
  private static final String SRC = "https://asset.symphony.com/symphony/audio/chime.mp3";
  private static final String ATTR_AUTOPLAY = "autoplay";

  public Chime(Element parent, FormatEnum format) throws InvalidInputException {
    super(parent, MESSAGEML_TAG, format);

    if (parent instanceof MessageML) {
      ((MessageML) parent).setChime(true);
    } else {
      throw new InvalidInputException("Element \"chime\" has to be the only element in the message.");
    }
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ATTR_SRC:
        if (!getStringAttribute(item).equalsIgnoreCase(SRC)) {
          throw new InvalidInputException("Attribute \"" + ATTR_SRC
              + "\" value needs to be \"" + SRC + "\"");
        }
        break;
      case ATTR_AUTOPLAY:
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    out.printElement(PRESENTATIONML_TAG, null, ATTR_SRC, SRC,
        ATTR_AUTOPLAY, "true");
  }

  @Override
  public String asText() {
    return "";
  }

  @Override
  public void validate() throws InvalidInputException {
    assertNoContent();
  }

  @Override
  public String getPresentationMLTag() {
    return PRESENTATIONML_TAG;
  }
}
