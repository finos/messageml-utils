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


import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.form.SelectNode;
import org.w3c.dom.Node;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * This class represents the Symphony Element Dialog which is represented with tag name "select" (drop down menu).
 * The messageML representation of the select element can contain the following attributes:
 * <ul>
 *   <li> name (required) -> to identify this element</li>
 *   <li> required -> true/false, to enforce a non empty option to be selected</li>
 *   <li> data-placeholder -> string, text displayed in the dropdown menu before an option is selected</li>
 *   <li> title -> string, the description that will be displayed when clicking the tooltip icon</li>
 *   <li> label -> string, definition of the label that will be displayed on top of the Masked Text Field Element</li>
 *   <li> multiple -> true/false, to allow for multiple options to be selected</li>
 *   <li> min -> integer, minimum number of options to select if multiple=true</li>
 *   <li> max -> integer, maximum number of options to select if multiple=true</li>
 * </ul>
 * It can contain the following child tags:
 * <ul>
 *   <li> option (required) -> the possible options to select {@link Option}</li>
 * </ul>
 */

public class Select extends FormElement implements LabelableElement, TooltipableElement {

  public static final String MESSAGEML_TAG = "select";
  public static final String ELEMENT_ID = "dropdown";
  private static final String REQUIRED_ATTR = "required";
  private static final String OPTION_SELECTED_ATTR = "selected";
  private static final String DATA_PLACEHOLDER_ATTR = "data-placeholder";
  private static final String MULTIPLE_ATTR = "multiple";
  private static final String MML_MIN_ATTR = "min";
  private static final String MIN_ATTR = "data-min";
  private static final String MML_MAX_ATTR = "max";
  private static final String MAX_ATTR = "data-max";
  protected static final String DISABLED_ATTR = "disabled";
  protected static final String READONLY_ATTR = "readonly";
  private static final String AUTO_SUBMIT_ATTR = "data-auto-submit";
  private static final String MML_AUTO_SUBMIT_ATTR = "auto-submit";

  public Select(Element parent) {
    super(parent, MESSAGEML_TAG);
  }

  @Override
  public org.commonmark.node.Node asMarkdown() {
    return new SelectNode(getAttribute(DATA_PLACEHOLDER_ATTR), getAttribute(LABEL), getAttribute(TITLE));
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    if (getAttribute(NAME_ATTR) == null) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }
    assertAttributeNotBlank(NAME_ATTR);

    assertContentModel(Collections.singleton(Option.class));
    assertContainsChildOfType(Collections.singleton(Option.class));

    if (getAttribute(REQUIRED_ATTR) != null) {
      assertAttributeValue(REQUIRED_ATTR, Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    if (getAttribute(MULTIPLE_ATTR) != null) {
      assertAttributeValue(MULTIPLE_ATTR, Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    if (getAttribute(DISABLED_ATTR) != null) {
      assertAttributeValue(DISABLED_ATTR,
          Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    if (getAttribute(READONLY_ATTR) != null) {
      assertAttributeValue(READONLY_ATTR,

          Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    if (getAttribute(AUTO_SUBMIT_ATTR) != null) {
      assertAttributeValue(AUTO_SUBMIT_ATTR,
          Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));
    }

    boolean multipleAttributeValue = Boolean.parseBoolean(getAttribute(MULTIPLE_ATTR));
    if (getAttribute(MIN_ATTR) != null && !multipleAttributeValue) {
      throw new InvalidInputException("Attribute \"min\" is not allowed. Attribute \"multiple\" missing");
    }

    if (getAttribute(MAX_ATTR) != null && !multipleAttributeValue) {
      throw new InvalidInputException("Attribute \"max\" is not allowed. Attribute \"multiple\" missing");
    }

    int min = checkIntegerAttribute(MIN_ATTR, 0, "Attribute \"min\" is not valid, it must be >= 0");
    int max = checkIntegerAttribute(MAX_ATTR, 1, "Attribute \"max\" is not valid, it must be >= 1");
    if (max > 0 && min > max) {
      throw new InvalidInputException("Attribute \"min\" is greater than attribute \"max\"");
    }

    if (multipleAttributeValue && Boolean.parseBoolean(getAttribute(REQUIRED_ATTR))
        && getAttribute(MIN_ATTR) != null && min == 0) {
      // multiple=true required=true min=0
      throw new InvalidInputException("Attribute \"min\" cannot be 0 if \"required\" is true");
    }

    if (!multipleAttributeValue) {
      assertOnlyOneOptionSelected();
    }
  }

  @Override
  protected void buildAttribute(MessageMLParser parser, Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case NAME_ATTR:
      case REQUIRED_ATTR:
      case DATA_PLACEHOLDER_ATTR:
      case LABEL:
      case TITLE:
      case MULTIPLE_ATTR:
      case DISABLED_ATTR:
      case READONLY_ATTR:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      case MML_MIN_ATTR:
        setAttribute(MIN_ATTR, getStringAttribute(item));
        break;
      case MML_MAX_ATTR:
        setAttribute(MAX_ATTR, getStringAttribute(item));
        break;
      case MML_AUTO_SUBMIT_ATTR:
        setAttribute(AUTO_SUBMIT_ATTR, getStringAttribute(item));
        break;
      case FORMNOVALIDATE_ATTR:
        setAttribute(FORMNOVALIDATE_PML_ATTR, getStringAttribute(item));
        break;
      case ID_ATTR:
      case FORMNOVALIDATE_PML_ATTR:
        if (format != FormatEnum.PRESENTATIONML) {
          throwInvalidInputException(item);
        }
        fillAttributes(parser, item);
        break;
      default:
        throwInvalidInputException(item);
    }
  }

  @Override
  public String getElementId() {
    return ELEMENT_ID;
  }

  @Override
  public void updateBiContext(BiContext context) {
    Map<String, Object> attributesMapBi = new HashMap<>();

    this.putOneIfPresent(attributesMapBi, BiFields.TITLE.getValue(), TITLE);
    this.putOneIfPresent(attributesMapBi, BiFields.LABEL.getValue(), LABEL);
    this.putOneIfPresent(attributesMapBi, BiFields.PLACEHOLDER.getValue(), DATA_PLACEHOLDER_ATTR);
    this.putOneIfPresent(attributesMapBi, BiFields.REQUIRED.getValue(), REQUIRED_ATTR);
    this.putOneIfPresent(attributesMapBi, BiFields.MULTI_SELECT.getValue(), MULTIPLE_ATTR);

    attributesMapBi.put(BiFields.OPTIONS_COUNT.getValue(), countChildrenOfType(Option.class));
    attributesMapBi.put(BiFields.DEFAULT.getValue(), isAtLeastOneOptionSelected());

    context.addItem(new BiItem(BiFields.SELECT.getValue(), attributesMapBi));
  }

  private int isAtLeastOneOptionSelected() {
    return getChildren().stream()
            .anyMatch(child -> child.getAttribute(OPTION_SELECTED_ATTR) != null) ? 1 : 0;
  }

  private void assertOnlyOneOptionSelected() throws InvalidInputException {
    long numberOfSelectedOptions = getChildren().stream()
            .map(child -> child.getAttribute(OPTION_SELECTED_ATTR))
            .filter(selectedAttr -> selectedAttr != null && selectedAttr.equalsIgnoreCase(Boolean.TRUE.toString()))
            .count();

    if (numberOfSelectedOptions > 1) {
      throw new InvalidInputException("Element \"select\" can only have one selected \"option\"");
    }
  }
}
