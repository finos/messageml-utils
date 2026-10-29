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

package org.finos.symphony.messageml.messagemlutils.compose.parser;

import org.commonmark.node.CustomNode;

/**
 * A Symphony semantic entity recognised in the Markdown source, held as an AST node rather than
 * rewritten in already-rendered text.
 *
 * <p>Detecting these on the AST is not a style preference. A defect existed precisely because the
 * mention pattern was applied to {@code Text} literals after commonmark had already consumed the link
 * syntax. Working at the node level also gets code spans, fenced code and link destinations excluded for
 * free: those are never handed to an inline content parser.
 *
 * <p>The node carries its own source text so the renderer can put it back verbatim. That is what makes
 * the per-agent toggle possible without parsing twice: the parse always recognises the form and
 * {@link MessageMLRenderOptions} decides at render time whether it becomes an element or the characters
 * the model actually wrote.
 */
public class SymphonyEntityNode extends CustomNode {

  /** Which Symphony element the source form maps to. */
  public enum Kind {
    HASH("hash"),
    CASH("cash");

    private final String element;

    Kind(String element) {
      this.element = element;
    }

    public String element() {
      return element;
    }
  }

  private final Kind kind;
  private final String value;
  private final String source;
  private final boolean explicit;

  /**
   * @param kind     the target element
   * @param value    the tag value, already stripped of sigils and trailing sentence punctuation
   * @param source   the exact source text this node replaced, used when the form is not enabled
   * @param explicit {@code true} for the bracketed forms {@code #[tag]} / {@code $[TICKER]}, which are
   *                 gated on the extended entity syntax; {@code false} for the bare forms, which are
   *                 always recognised because no toggle can stop a model from emitting them
   */
  public SymphonyEntityNode(Kind kind, String value, String source, boolean explicit) {
    this.kind = kind;
    this.value = value;
    this.source = source;
    this.explicit = explicit;
  }

  public Kind kind() {
    return kind;
  }

  public String value() {
    return value;
  }

  public String source() {
    return source;
  }

  public boolean explicit() {
    return explicit;
  }
}
