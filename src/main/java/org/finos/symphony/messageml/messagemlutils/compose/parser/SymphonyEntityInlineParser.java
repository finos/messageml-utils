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

import org.commonmark.parser.beta.InlineContentParser;
import org.commonmark.parser.beta.InlineContentParserFactory;
import org.commonmark.parser.beta.InlineParserState;
import org.commonmark.parser.beta.ParsedInline;
import org.commonmark.parser.beta.Position;
import org.commonmark.parser.beta.Scanner;
import org.finos.symphony.messageml.messagemlutils.elements.HashTag;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Recognises the Symphony topic and instrument forms as commonmark inline content, replacing the regex
 * pass that used to run over rendered text.
 *
 * <p>Four forms:
 *
 * <ul>
 *   <li>{@code #[quarterly-review]} and {@code $[AAPL]} - the explicit forms the output-format contract
 *       teaches, gated on the per-agent entity syntax at render time;</li>
 *   <li>bare {@code #compliance} and {@code $AAPL} - always recognised, because a model emits them
 *       unprompted and no toggle can prevent that.</li>
 * </ul>
 *
 * <p>The bare rules are deliberately tight: a bare token may not start with a digit
 * ({@code #123} and {@code #2.1} stay plain text), a bare instrument must be a 2-5 character uppercase
 * ticker ({@code $12} and {@code $msft} stay plain text) and a bare token may not <em>end</em> with
 * {@code .} or {@code -} so a sentence-ending period is not absorbed into the tag. Interior separators
 * remain legal: {@code #version.1.0} and {@code #my-tag} still work.
 *
 * <p>Anything that does not match exactly is left alone - the scanner is rewound and the characters stay
 * literal text. That is the safe degradation the whole strategy depends on: a hallucinated sigil costs a
 * few characters of prose, never a rejected message.
 */
public class SymphonyEntityInlineParser implements InlineContentParser {

  private static final char HASH_SIGIL = '#';
  private static final char CASH_SIGIL = '$';
  private static final int MIN_TICKER_LENGTH = 2;
  private static final int MAX_TICKER_LENGTH = 5;

  /** Registers this parser for both sigils. Stateless, so one instance per parse is enough. */
  public static class Factory implements InlineContentParserFactory {

    @Override
    public Set<Character> getTriggerCharacters() {
      return Set.of(HASH_SIGIL, CASH_SIGIL);
    }

    @Override
    public InlineContentParser create() {
      return new SymphonyEntityInlineParser();
    }
  }

  @Override
  public ParsedInline tryParse(InlineParserState state) {
    Scanner scanner = state.scanner();
    Position start = scanner.position();

    if (!atTokenStart(scanner)) {
      return none(scanner, start);
    }
    char sigil = scanner.peek();
    scanner.next();

    ParsedInline parsed = scanner.peek() == '['
        ? parseExplicit(scanner, sigil)
        : parseBare(scanner, sigil);
    return parsed == null ? none(scanner, start) : parsed;
  }

  /**
   * A sigil only opens a form at the start of the content or after whitespace, matching the behaviour
   * the previous regex had. Without this, {@code a#b} and an email's {@code $} would both be candidates.
   */
  private static boolean atTokenStart(Scanner scanner) {
    int previous = scanner.peekPreviousCodePoint();
    return previous < 0 || previous == Scanner.END || Character.isWhitespace(previous);
  }

  /** {@code #[tag]} / {@code $[TICKER]}. Returns {@code null} when the form is not well formed. */
  private static ParsedInline parseExplicit(Scanner scanner, char sigil) {
    scanner.next();
    StringBuilder value = new StringBuilder();
    while (scanner.hasNext()) {
      char c = scanner.peek();
      if (c == ']') {
        scanner.next();
        String tag = value.toString();
        if (tag.isEmpty() || !isValidTag(tag)) {
          return null;
        }
        return ParsedInline.of(
            new SymphonyEntityNode(kindOf(sigil), tag, sigil + "[" + tag + "]", true),
            scanner.position());
      }
      if (c == '[' || Character.isWhitespace(c)) {
        return null;
      }
      value.append(c);
      scanner.next();
    }
    return null;
  }

  /** Bare {@code #compliance} / {@code $AAPL}. Returns {@code null} when the token is not a tag. */
  private static ParsedInline parseBare(Scanner scanner, char sigil) {
    return sigil == HASH_SIGIL ? parseBareHash(scanner) : parseBareCash(scanner);
  }

  private static ParsedInline parseBareHash(Scanner scanner) {
    char first = scanner.peek();
    if (!(Character.isLetter(first) || first == '_')) {
      return null;
    }
    StringBuilder token = new StringBuilder();
    // A position is recorded after every accepted character so the token can be shortened afterwards:
    // trailing '.' or '-' belongs to the sentence, not to the tag.
    List<Position> positions = new ArrayList<>();
    while (scanner.hasNext() && isHashTagChar(scanner.peek())) {
      token.append(scanner.peek());
      scanner.next();
      positions.add(scanner.position());
    }
    int end = token.length();
    while (end > 0 && isTrailingSeparator(token.charAt(end - 1))) {
      end--;
    }
    if (end == 0) {
      return null;
    }
    String tag = token.substring(0, end);
    if (!isValidTag(tag)) {
      return null;
    }
    scanner.setPosition(positions.get(end - 1));
    return ParsedInline.of(
        new SymphonyEntityNode(SymphonyEntityNode.Kind.HASH, tag, HASH_SIGIL + tag, false),
        scanner.position());
  }

  private static ParsedInline parseBareCash(Scanner scanner) {
    StringBuilder token = new StringBuilder();
    while (scanner.hasNext() && token.length() <= MAX_TICKER_LENGTH
        && scanner.peek() >= 'A' && scanner.peek() <= 'Z') {
      token.append(scanner.peek());
      scanner.next();
    }
    if (token.length() < MIN_TICKER_LENGTH || token.length() > MAX_TICKER_LENGTH) {
      return null;
    }
    // A longer word that merely starts with uppercase letters is not a ticker.
    if (scanner.hasNext() && isTickerBoundaryViolation(scanner.peek())) {
      return null;
    }
    String tag = token.toString();
    if (!isValidTag(tag)) {
      return null;
    }
    return ParsedInline.of(
        new SymphonyEntityNode(SymphonyEntityNode.Kind.CASH, tag, CASH_SIGIL + tag, false),
        scanner.position());
  }

  private static boolean isHashTagChar(char c) {
    return Character.isLetterOrDigit(c) || c == '_' || c == '.' || c == '-';
  }

  private static boolean isTrailingSeparator(char c) {
    return c == '.' || c == '-';
  }

  private static boolean isTickerBoundaryViolation(char c) {
    return Character.isLetterOrDigit(c) || c == '_';
  }

  /**
   * A tag value must satisfy the pattern messageml-utils enforces on {@code <hash>}. {@code <cash>}
   * carries no library-side pattern at all - {@code CashTag} inherits {@code Keyword.validate()}, which
   * only requires the attribute to be present - so the same check is applied to both. It is permissive
   * by design: it rejects a candidate made entirely of punctuation, which is the case that would
   * otherwise produce a message Symphony rejects.
   */
  private static boolean isValidTag(String tag) {
    return !tag.isBlank() && tag.matches(HashTag.HASHTAG_PATTERN);
  }

  private static SymphonyEntityNode.Kind kindOf(char sigil) {
    return sigil == HASH_SIGIL ? SymphonyEntityNode.Kind.HASH : SymphonyEntityNode.Kind.CASH;
  }

  private static ParsedInline none(Scanner scanner, Position start) {
    scanner.setPosition(start);
    return ParsedInline.none();
  }
}
