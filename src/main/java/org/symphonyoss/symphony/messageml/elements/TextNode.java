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
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Text;

/**
 * Class representing text content.
 *
 * @author lukasz
 * @since 3/27/17
 */
public class TextNode extends Element {
  private String text;

  public TextNode(Element parent, Text node) {
    this(parent, removeNewLines(node.getTextContent()));
  }

  public TextNode(Element parent, String text) {
    super(-1, parent);
    this.text = text;
  }

  private static String removeNewLines(String textContent) {
    if (textContent == null) {
      return "";
    }

    StringBuilder s = new StringBuilder();
    boolean inNl = false;

    for (char c : textContent.toCharArray()) {
      if (c == '\n') {
        if (!inNl) {
          s.append(' ');
          inNl = true;
        }
      } else {
        inNl = false;
        s.append(c);
      }
    }
    return s.toString();
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    out.println(out.escape(this.text));
  }

  public void asMarkdown(org.commonmark.node.Node parent) {
    org.commonmark.node.Text node = new org.commonmark.node.Text(this.text);
    parent.appendChild(node);
  }

  @Override
  protected void buildText(Text node) {
    this.text = removeNewLines(node.getTextContent());
  }

  public String getText() {
    return this.text;
  }

  public void setText(String text) {
    this.text = text;
  }

  @Override
  public String asText() {
    return this.text;
  }

  @Override
  public Node asMarkdown() {
    return new org.commonmark.node.Text(this.text);
  }

  @Override
  public String toString() {
    return "Text(" + this.text + ")";
  }
}
