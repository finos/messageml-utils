/*
 * Copyright 2026 MessageML - Symphony LLC
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

package org.finos.symphony.messageml.messagemlutils.compose;

import org.commonmark.node.Node;
import org.commonmark.node.Paragraph;
import org.commonmark.node.Text;
import org.commonmark.parser.Parser;

import java.util.Set;

/**
 * Turns an HTML named character reference ({@code &nbsp;}, {@code &mdash;}, {@code &rarr;}) into
 * something XML understands. MessageML is parsed as XML, which only predefines {@code amp},
 * {@code lt}, {@code gt}, {@code quot} and {@code apos}, so every other name a model borrows from HTML
 * fails the whole block with "The entity ... was referenced, but not declared".
 *
 * <p>The HTML5 name table is not retyped here. commonmark already ships it and decodes references
 * in Markdown text through its public parser, so a one-reference document is parsed and the decoded
 * literal read back. That keeps the whole HTML5 set (2000+ names) without depending on commonmark's
 * internal packages. It only runs for a reference that is neither predefined nor numeric, which is
 * rare enough that the per-call parse does not matter.
 *
 * <p>Stateless and safe to share across threads ({@link Parser} is thread-safe).
 */
final class HtmlEntities {

  /** The five names XML predefines, kept as written. */
  static final Set<String> XML_PREDEFINED = Set.of("amp", "lt", "gt", "quot", "apos");

  private static final Parser PARSER = Parser.builder().build();

  private HtmlEntities() {
  }

  /**
   * @param name the reference name without {@code &} and {@code ;}
   * @return the reference rewritten as numeric character references ({@code &#8212;}) or
   *     {@code null} when {@code name} is not an HTML5 named reference
   */
  static String toNumericReference(String name) {
    String reference = "&" + name + ";";
    Node paragraph = PARSER.parse(reference).getFirstChild();
    if (!(paragraph instanceof Paragraph) || !(paragraph.getFirstChild() instanceof Text text)
        || text.getNext() != null || text.getLiteral().equals(reference)) {
      return null;
    }
    StringBuilder numeric = new StringBuilder();
    text.getLiteral().codePoints().forEach(cp -> numeric.append("&#").append(cp).append(';'));
    return numeric.toString();
  }
}
