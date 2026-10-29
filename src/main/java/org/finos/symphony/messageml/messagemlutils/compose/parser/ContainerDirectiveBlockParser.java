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

import org.commonmark.node.Block;
import org.commonmark.parser.SourceLine;
import org.commonmark.parser.block.AbstractBlockParser;
import org.commonmark.parser.block.BlockContinue;
import org.commonmark.parser.block.BlockParserFactory;
import org.commonmark.parser.block.BlockStart;
import org.commonmark.parser.block.MatchedBlockParser;
import org.commonmark.parser.block.ParserState;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses the {@code :::} container directives as a commonmark block.
 *
 * <p>Accepted openings, parsed tolerantly:
 *
 * <pre>
 * :::card                          ::: card
 * :::card accent=blue icon=https://example.com/i.png
 * :::expand Quarterly detail       :::expandable Quarterly detail
 * :::expand[expanded] Title        :::expand[cropped] Title      :::expand[error] Title
 * </pre>
 *
 * <p>Lines are collected verbatim until a line consisting of {@code :::} closes the block or until the
 * document ends - which is what makes a truncated streaming buffer safe. Body content is deliberately not
 * parsed here; see {@link ContainerDirective}.
 */
class ContainerDirectiveBlockParser extends AbstractBlockParser {

  private static final Pattern OPENING = Pattern.compile(
      "^:::[ \\t]*(card|expandable|expand)(?:\\[([a-zA-Z]+)])?[ \\t]*(.*)$");
  private static final Pattern CLOSING = Pattern.compile("^:::[ \\t]*$");
  /** A value is double-quoted, single-quoted (both may hold spaces) or bare. */
  private static final Pattern ATTRIBUTE = Pattern.compile("([a-zA-Z]+)=(\"[^\"]*\"|'[^']*'|\\S+)");
  /** Our own allowlist: messageml-utils validates neither accent values nor icon absoluteness. */
  private static final Set<String> ALLOWED_ATTRIBUTES = Set.of("accent", "icon", "state", "variant");
  private static final Set<String> ALLOWED_STATES = Set.of("collapsed", "cropped", "expanded");
  private static final Set<String> ALLOWED_VARIANTS = Set.of("default", "error");
  private static final Pattern ACCENT_VALUE = Pattern.compile("[a-z]+");
  private static final Pattern ABSOLUTE_URI = Pattern.compile("(?i)[a-z][a-z0-9+.\\-]*://.+");

  private final ContainerDirective block;

  private ContainerDirectiveBlockParser(ContainerDirective block) {
    this.block = block;
  }

  @Override
  public Block getBlock() {
    return block;
  }

  @Override
  public BlockContinue tryContinue(ParserState state) {
    CharSequence line = state.getLine().getContent();
    if (CLOSING.matcher(line.toString()).matches()) {
      block.markClosed();
      return BlockContinue.finished();
    }
    return BlockContinue.atIndex(state.getIndex());
  }

  @Override
  public void addLine(SourceLine line) {
    block.addBodyLine(line.getContent().toString());
  }

  static class Factory implements BlockParserFactory {

    @Override
    public BlockStart tryStart(ParserState state, MatchedBlockParser matchedBlockParser) {
      // An indented ':::' is code, not a directive - the same four-space rule commonmark applies
      // everywhere else.
      if (state.getIndent() >= 4) {
        return BlockStart.none();
      }
      String line = state.getLine().getContent().toString().substring(state.getNextNonSpaceIndex());
      Matcher matcher = OPENING.matcher(line);
      if (!matcher.matches()) {
        return BlockStart.none();
      }
      return BlockStart.of(new ContainerDirectiveBlockParser(directiveFrom(matcher, line)))
          .atIndex(state.getLine().getContent().length());
    }
  }

  private static ContainerDirective directiveFrom(Matcher matcher, String openingLine) {
    ContainerDirective.Kind kind = "card".equals(matcher.group(1))
        ? ContainerDirective.Kind.CARD
        : ContainerDirective.Kind.EXPAND;
    String modifier = matcher.group(2) == null ? null : matcher.group(2).toLowerCase();
    String rest = matcher.group(3) == null ? "" : matcher.group(3).trim();

    Map<String, String> attributes = new LinkedHashMap<>();
    String title = extractAttributes(rest, attributes);

    String state = resolveState(modifier, attributes.get("state"));
    String variant = resolveVariant(modifier, attributes.get("variant"));
    return new ContainerDirective(kind, openingLine, title.isEmpty() ? null : title, state, variant,
        allowedAccent(attributes.get("accent")), allowedIcon(attributes.get("icon")));
  }

  /**
   * Pulls leading {@code key=value} tokens off the opening line and returns whatever text remains as the
   * directive title. An attribute name outside the allowlist is dropped and its value with it, rather
   * than becoming part of the title - dropping the pair is what the spec asks for and keeping it would
   * put {@code onclick=…} in a header.
   */
  private static String extractAttributes(String rest, Map<String, String> attributes) {
    String remaining = rest;
    while (!remaining.isEmpty()) {
      Matcher attribute = ATTRIBUTE.matcher(remaining);
      if (!attribute.lookingAt()) {
        break;
      }
      String name = attribute.group(1).toLowerCase();
      if (ALLOWED_ATTRIBUTES.contains(name)) {
        attributes.put(name, unquote(attribute.group(2)));
      }
      remaining = remaining.substring(attribute.end()).stripLeading();
    }
    return remaining;
  }

  private static String unquote(String value) {
    boolean quoted = value.length() >= 2
        && (value.charAt(0) == '"' || value.charAt(0) == '\'')
        && value.charAt(value.length() - 1) == value.charAt(0);
    return quoted ? value.substring(1, value.length() - 1) : value;
  }

  /** The bracket modifier wins over an explicit {@code state=} attribute: it is the documented form. */
  private static String resolveState(String modifier, String attribute) {
    if (modifier != null && ALLOWED_STATES.contains(modifier)) {
      return modifier;
    }
    return attribute != null && ALLOWED_STATES.contains(attribute) ? attribute : null;
  }

  private static String resolveVariant(String modifier, String attribute) {
    if ("error".equals(modifier)) {
      return "error";
    }
    return attribute != null && ALLOWED_VARIANTS.contains(attribute) ? attribute : null;
  }

  private static String allowedAccent(String value) {
    return value != null && ACCENT_VALUE.matcher(value).matches() ? value : null;
  }

  /** {@code iconSrc} is rendered as a URL, so a relative value would resolve against the client. */
  private static String allowedIcon(String value) {
    return value != null && ABSOLUTE_URI.matcher(value).matches() ? value : null;
  }
}
