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

import org.apache.commons.lang3.StringUtils;
import org.commonmark.node.FencedCodeBlock;
import org.commonmark.node.Node;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.bi.BiContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.util.XmlPrintStream;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Class representing a block container for block or inline content.
 * @author lukasz
 * @since 3/27/17
 */
public class Code extends Element {

  public static final String MESSAGEML_TAG = "code";
  private static final String MML_LANGUAGE_ATTR = "language";
  private static final String PML_LANGUAGE_ATTR = "data-language";
  public static final char MARKDOWN_DELIMITER_CHAR = '`';
  public static final int MARKDOWN_DELIMITER_LENGTH = 3;
  private static final int MARKDOWN_DELIMITER_INDENT = 0;

  private static final List<String> SUPPORTED_LANGUAGES = Arrays.asList(
      "plaintext", "c", "cpp", "csharp", "css", "html", "java", "js", "jsx", "php", "python", "r",
      "typescript", "tsx", "markdown", "json", "scala", "shell", "yaml"
  );

  public Code(Element parent) {
    super(parent, MESSAGEML_TAG);
  }
  public Code(Element parent, String language) {
    super(parent, MESSAGEML_TAG);
    this.setAttribute(PML_LANGUAGE_ATTR, language);
  }

  @Override
  void buildAttribute(MessageMLParser parser, org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case MML_LANGUAGE_ATTR:
      case PML_LANGUAGE_ATTR:
        setAttribute(item.getNodeName(), getStringAttribute(item));
        break;
      default:
        super.buildAttribute(parser, item);
    }
  }

  @Override
  void asPresentationML(XmlPrintStream out, MessageMLContext context) {
    final Map<String, Object> attrs = new HashMap<>();

    if (getAttribute(MML_LANGUAGE_ATTR) != null) {
      attrs.put(PML_LANGUAGE_ATTR, getAttribute(MML_LANGUAGE_ATTR));
    }

    if (getAttribute(PML_LANGUAGE_ATTR) != null) {
      attrs.put(PML_LANGUAGE_ATTR, getAttribute(PML_LANGUAGE_ATTR));
    }

    out.openElement(MESSAGEML_TAG, attrs);

    for (Element child : getChildren()) {
      child.asPresentationML(out, context);
    }

    out.closeElement();
  }

  @Override
  public Node asMarkdown() {
    final FencedCodeBlock node = new FencedCodeBlock();
    node.setFenceChar(MARKDOWN_DELIMITER_CHAR);
    node.setFenceLength(MARKDOWN_DELIMITER_LENGTH);
    node.setFenceIndent(MARKDOWN_DELIMITER_INDENT);

    if (getAttribute(MML_LANGUAGE_ATTR) != null) {
      node.setInfo(getAttribute(MML_LANGUAGE_ATTR));
    }

    if (getAttribute(PML_LANGUAGE_ATTR) != null) {
      node.setInfo(getAttribute(PML_LANGUAGE_ATTR));
    }

    return node;
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();
    if (StringUtils.isNotEmpty(getAttribute(MML_LANGUAGE_ATTR))) {
      assertAttributeValue(MML_LANGUAGE_ATTR, SUPPORTED_LANGUAGES);
    }
    if (StringUtils.isNotEmpty(getAttribute(PML_LANGUAGE_ATTR))) {
      assertAttributeValue(PML_LANGUAGE_ATTR, SUPPORTED_LANGUAGES);
    }
    assertPreformattedOrPhrasingContent();
  }

  @Override
  public String getPresentationMLTag() {
    return super.getPresentationMLTag();
  }

  @Override
  void updateBiContext(BiContext context) {
    super.updateBiContext(context);
    context.updateItemCount(BiFields.CODE.getValue());
  }
}
