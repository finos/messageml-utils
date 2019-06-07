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
import org.symphonyoss.symphony.messageml.markdown.nodes.form.TableSelectNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * Class representing a TableSelect inside a Form.
 *
 * @author Pedro Sanchez
 * @since 05/30/2019
 */
public class TableSelect extends FormElement {

  public static final String MESSAGEML_TAG = "tableselect";

  private static final String NAME_ATTR = "name";
  private static final String TYPE_ATTR = "type";
  private static final String HEADER_TEXT_ATTR = "header-text";
  private static final String BUTTON_TEXT_ATTR = "button-text";
  private static final String POSITION_ATTR = "position";

  private static final String PRESENTATIONML_TABLE_TAG = "table";

  public static final Set<String> VALID_TYPES = new HashSet<>(Arrays.asList("button", "checkbox"));

  public static final Set<String> VALID_POSITIONS = new HashSet<>(Arrays.asList("left", "right"));

  private final static String LEFT = "left";
  private final static String RIGHT = "right";
  private final static String BUTTON = "button";
  private final static String CHECKBOX = "checkbox";

  public TableSelect(Element parent) {
    super(parent, MESSAGEML_TAG);

    setAttribute(HEADER_TEXT_ATTR, "Select");
    setAttribute(BUTTON_TEXT_ATTR, "SELECT");
  }


  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    String name = getAttribute(NAME_ATTR);

    if (name == null || name.trim().isEmpty()) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }

    String type = getAttribute(TYPE_ATTR);

    if (type == null) {
      throw new InvalidInputException("The attribute \"type\" is required");
    }

    if (!VALID_TYPES.contains(type)) {
      throw new InvalidInputException("Attribute \"type\" must be \"button\" or \"checkbox\"");
    }


    String position = getAttribute(POSITION_ATTR);


    if (position == null) {
      throw new InvalidInputException("The attribute \"position\" is required");
    }

    if (!VALID_POSITIONS.contains(position)) {
      throw new InvalidInputException("Attribute \"position\" must be \"left\" or \"right\"");
    }

    assertContentModel(Arrays.asList(TableHeader.class, TableBody.class, TableFooter.class, TableRow.class));
  }

  @Override
  protected void buildAttribute(org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {

      case NAME_ATTR:
        setAttribute(NAME_ATTR, getStringAttribute(item));
        break;

      case TYPE_ATTR:
        setAttribute(TYPE_ATTR, getStringAttribute(item));
        break;

      case HEADER_TEXT_ATTR:
        setAttribute(HEADER_TEXT_ATTR, getStringAttribute(item));
        break;

      case BUTTON_TEXT_ATTR:
        setAttribute(BUTTON_TEXT_ATTR, getStringAttribute(item));
        break;

      case POSITION_ATTR:
        setAttribute(POSITION_ATTR, getStringAttribute(item));
        break;

      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName()
            + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    out.openElement(PRESENTATIONML_TABLE_TAG);
    for (Element child : getChildren()) {
      child.asPresentationML(out);
    }
    out.closeElement(); // Closing table
  }

  @Override
  public Node asMarkdown() {
    return new TableSelectNode();
  }

  public String getName() {
    return getAttribute(NAME_ATTR);
  }

  public String getPosition() {
    return getAttribute(POSITION_ATTR);
  }

  public String getType() {
    return getAttribute(TYPE_ATTR);
  }

  public String getHeaderText() {
    return getAttribute(HEADER_TEXT_ATTR);
  }

  public String getButtonText() {
    return getAttribute(BUTTON_TEXT_ATTR);
  }

  public Boolean isRight() {
    return RIGHT.equals(getPosition());
  }

  public Boolean isLeft() {
    return LEFT.equals(getPosition());
  }

  public Boolean isButton() {
    return BUTTON.equals(getType());
  }

  public Boolean isCheckbox() {
    return CHECKBOX.equals(getType());
  }

}
