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
 * A {@code :::card} or {@code :::expand} container directive, as parsed from the Markdown source.
 *
 * <p>The body is kept as raw source lines rather than as parsed children, for three reasons that all
 * matter:
 *
 * <ul>
 *   <li>with content grouping disabled the directive must render as literal text and the exact
 *       characters the model wrote are the only faithful way to do that;</li>
 *   <li>the body is re-parsed by a parser that does <em>not</em> know about directives, which is what
 *       makes nesting impossible by construction - an inner {@code :::card} comes back as literal text
 *       instead of a nested card Symphony would reject;</li>
 *   <li>a container whose closing {@code :::} has not been generated yet simply has fewer lines, so a
 *       truncated streaming buffer still renders a valid card.</li>
 * </ul>
 */
public class ContainerDirective extends CustomBlock {

  /** Which Symphony grouping element the directive maps to. */
  public enum Kind {
    CARD,
    EXPAND
  }

  private final Kind kind;
  private final String openingLine;
  private final String title;
  private final String authoredState;
  private final String bodyVariant;
  private final String accent;
  private final String iconSrc;
  private final List<String> bodyLines = new ArrayList<>();
  private boolean closed;

  ContainerDirective(Kind kind, String openingLine, String title, String authoredState,
      String bodyVariant, String accent, String iconSrc) {
    this.kind = kind;
    this.openingLine = openingLine;
    this.title = title;
    this.authoredState = authoredState;
    this.bodyVariant = bodyVariant;
    this.accent = accent;
    this.iconSrc = iconSrc;
  }

  public Kind kind() {
    return kind;
  }

  /** The opening line verbatim, for the content-grouping-disabled path. */
  public String openingLine() {
    return openingLine;
  }

  /** Title text from the opening line or {@code null}. */
  public String title() {
    return title;
  }

  /** {@code collapsed} / {@code cropped} / {@code expanded} as authored or {@code null}. */
  public String authoredState() {
    return authoredState;
  }

  /** {@code error} when the {@code [error]} modifier was used, otherwise {@code null}. */
  public String bodyVariant() {
    return bodyVariant;
  }

  public String accent() {
    return accent;
  }

  public String iconSrc() {
    return iconSrc;
  }

  public String body() {
    return String.join("\n", bodyLines);
  }

  /** {@code false} when the closing {@code :::} was never reached - a truncated streaming buffer. */
  public boolean closed() {
    return closed;
  }

  void addBodyLine(String line) {
    bodyLines.add(line);
  }

  void markClosed() {
    this.closed = true;
  }
}
