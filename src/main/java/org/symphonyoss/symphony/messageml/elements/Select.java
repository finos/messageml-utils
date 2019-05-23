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
import org.symphonyoss.symphony.messageml.markdown.nodes.SelectNode;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Class representing dropdown menu - Symphony Elements.
 *
 * @author lumoura
 * @since 3/22/18
 */
public class Select extends Element {

  public static final String MESSAGEML_TAG = "select";
  private static final String NAME_ATTR = "name";
  private static final String REQUIRED_ATTR = "required";
  private static final Set<String> VALID_VALUES_FOR_REQUIRED_ATTR = new HashSet<>(Arrays.asList("true", "false"));

  public Select(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new SelectNode(getAttribute(NAME_ATTR));
  }

  @Override
  public void validate() throws InvalidInputException {
    if (getAttribute(NAME_ATTR) == null) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }

    assertSingleParent(new Form(null));
    assertContentModel(Collections.singleton(Option.class));
    assertAtLeastOneOptionChild();
    validateRequiredAttribute(getAttribute(REQUIRED_ATTR));
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
        setAttribute(NAME_ATTR, getStringAttribute(item));
        break;
      case REQUIRED_ATTR:
        setAttribute(REQUIRED_ATTR, getStringAttribute(item));
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  private void assertAtLeastOneOptionChild() throws InvalidInputException {
    boolean hasOptionElementAsChild = this.getChildren()
        .stream()
        .anyMatch(child -> child.getClass().equals(Option.class));

    if (!hasOptionElementAsChild) {
      throw new InvalidInputException(String.format("The \"%s\" element must have at least one \"%s\" as its child.",
          MESSAGEML_TAG, Option.MESSAGEML_TAG));
    }
  }

  private void validateRequiredAttribute(String requiredAttrValue) throws InvalidInputException {
    if (requiredAttrValue != null && !VALID_VALUES_FOR_REQUIRED_ATTR.contains(requiredAttrValue)) {
      throw new InvalidInputException(String.format("Attribute \"%s\" must have one of the following values: %s",
          REQUIRED_ATTR, VALID_VALUES_FOR_REQUIRED_ATTR));
    }
  }
}
