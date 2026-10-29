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

import org.commonmark.node.CustomBlock;

import java.util.ArrayList;
import java.util.List;

/**
 * A top-level {@code <messageML>...</messageML>} region, as parsed from the Markdown source.
 *
 * <p>The body is kept as raw source lines, verbatim, for the same reasons {@link ContainerDirective}
 * keeps its body raw: it is the only faithful rendering when native MessageML is disabled and a
 * truncated streaming buffer with no closing tag yet still carries whatever content arrived so far.
 * Unlike {@link ContainerDirective}, the body is never re-parsed as Markdown - it is native MessageML,
 * handled by its own sanitize/balance/validate pipeline.
 */
public class MessageMLBlock extends CustomBlock {

  private final List<String> bodyLines = new ArrayList<>();
  private boolean closed;
  private String trailingText = "";

  /** The block content, verbatim, lines joined with {@code \n}. */
  public String body() {
    return String.join("\n", bodyLines);
  }

  /** {@code false} when the closing {@code </messageML>} was never reached - a truncated streaming buffer. */
  public boolean closed() {
    return closed;
  }

  /**
   * Whatever the model wrote after the closing {@code </messageML>} on that same line, stripped. It is
   * not part of the block and is meant to be rendered as Markdown right after it. Empty when there is
   * none or the block is not closed.
   */
  public String trailingText() {
    return trailingText;
  }

  void addBodyLine(String line) {
    bodyLines.add(line);
  }

  void markClosed(String trailingText) {
    this.closed = true;
    this.trailingText = trailingText.strip();
  }
}
