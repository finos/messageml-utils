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
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.FormNode;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.IUserPresentation;

import java.util.Collections;


/**
 * Class representing a Symphony Elements form
 *
 * @author lumoura
 * @since 05/02/19
 */
public class Form extends Element {

  public static final String MESSAGEML_TAG = "form";
  public static final String ID_ATTR = "id";
  public static final String ACTION_URL_ATTR = "action";

  private final IDataProvider dataProvider;

  public Form(Element parent, IDataProvider dataProvider) {
    super(parent, MESSAGEML_TAG);
    this.dataProvider = dataProvider;
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    if (item.getNodeName().equals(ID_ATTR)) {
      setAttribute(ID_ATTR, getStringAttribute(item));
    } else {
      throw new InvalidInputException("Attribute \"" + item.getNodeName()
              + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  @Override
  public Node asMarkdown() {
    return new FormNode();
  }

  private void validateActionButton() throws InvalidInputException {
    boolean valid = false;
    for (Element el : this.getChildren()) {
      if (el.getClass() == Button.class && el.getAttribute(Button.TYPE_ATTR).equals(Button.ACTION_TYPE)) {
        valid = true;
        break;
      }
    }
    if (!valid) {
      throw new InvalidInputException("The form must contain at least one action button");
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    String id = getAttribute(ID_ATTR);
    resolveUser();
    if (id == null) {
      throw new InvalidInputException("The attribute \"id\" is required");
    }
    validateActionButton();
    assertContentModel(Collections.<Class<? extends Element>>singleton(Button.class));
  }

  private void resolveUser() throws InvalidInputException {
    IUserPresentation userPresentation = dataProvider.getSenderPresentation();
    setAttribute(ACTION_URL_ATTR, userPresentation.getInteractionUrl());
  }
}
