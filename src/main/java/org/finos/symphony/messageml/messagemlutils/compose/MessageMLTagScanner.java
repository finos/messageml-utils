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

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * A small, deterministic tokenizer over a MessageML fragment, shared by {@link MessageMLSanitizer}
 * and {@link MessageMLBalancer}.
 *
 * <p>It recognises opening tags, closing tags, self-closing tags, text, comments ({@code <!-- -->})
 * and CDATA sections ({@code <![CDATA[ ]]>}) and tracks the offsets of quoted attribute values
 * separately so a caller can rewrite what is inside a tag's attributes without touching the tag or
 * attribute names themselves. A {@code >} inside a quoted attribute value never closes the tag.
 *
 * <p>A FreeMarker directive ({@code <#assign ...>}, {@code </#if>}) or macro call
 * ({@code <@macro/>}, {@code </@macro>}) is recognised as an ordinary tag whose name happens to start
 * with {@code #} or {@code @} - the scanner does not treat these specially, it just does not require
 * a tag name to start with a letter. It is not a FreeMarker guard either: a directive with a
 * {@code <} inside ({@code <#if a < b>}) falls under the literal-{@code <} rule below and comes out
 * as {@link Text}. {@link MessageMLSanitizer} rejects every {@code <#} and {@code <@} with a regex
 * over the raw string before anything is scanned. The other consumers only ever see what it let
 * through.
 *
 * <p>When the fragment ends mid-construct - an unfinished tag (including one left open by an
 * unterminated quoted attribute value, as long as no raw {@code <} follows the quote), an unfinished
 * comment or an unfinished CDATA section - scanning stops there rather than guessing.
 * {@link ScanResult#truncated()} is set and {@link ScanResult#consumedLength()} is the offset of the
 * start of that unfinished construct, so a caller that wants to cut the tail (the balancer's
 * first step) can do so without re-scanning.
 *
 * <p>A {@code <} that cannot start markup is kept as part of the surrounding {@link Text} instead:
 * one followed by a space, a digit or any other character that cannot start a tag name
 * ({@code if a < b}), one whose would-be tag runs into another {@code <} before any {@code >}
 * ({@code if a<b then c</p>} or {@code <img src="x > <p>} where the quote is never closed - XML
 * does not allow a raw {@code <} inside an attribute value) and a {@code <!} that opens neither a
 * comment nor CDATA.
 * Treating those as a truncation, as an earlier version did, silently cut everything after them
 * from a final answer. Only a {@code <} that could still become markup once more characters arrive
 * ({@code <}, {@code </} or {@code <!-} at the very end and a tag name with no {@code >} yet) counts
 * as truncated.
 * A trailing {@code &...} with no closing {@code ;} is text too, however much it looks like a
 * reference being typed ({@code &am}, {@code &#12}): the scanner cannot tell a live stream from a
 * finished answer and holding it back cut the {@code &T} off a final {@code AT&T} or the {@code &D}
 * off {@code R&D}. The balancer escapes it instead.
 * A {@link Text} token can therefore contain a raw {@code <} or {@code &}; escaping them is the
 * caller's job (see {@link MessageMLStructureRepairer}).
 */
public final class MessageMLTagScanner {

  public sealed interface Token permits OpenTag, CloseTag, Text, Comment, Cdata {
    int start();

    int end();
  }

  /**
   * @param attributeValueRanges the offsets of each quoted attribute value's content (quotes
   *                              excluded), in document order, relative to the fragment the scanner
   *                              was given
   */
  public record OpenTag(String name, boolean selfClosing, int start, int end, List<int[]> attributeValueRanges)
      implements Token {}

  public record CloseTag(String name, int start, int end) implements Token {}

  public record Text(int start, int end) implements Token {}

  public record Comment(int start, int end) implements Token {}

  public record Cdata(int start, int end) implements Token {}

  /**
   * @param tokens         every complete token found, in document order, covering
   *                       {@code [0, consumedLength)}
   * @param consumedLength how much of the fragment was fully tokenized; equal to the fragment's
   *                       length unless {@code truncated}
   * @param truncated      {@code true} when the fragment ends mid-construct
   */
  public record ScanResult(List<Token> tokens, int consumedLength, boolean truncated) {}

  private record TagScan(Token token, int end) {}

  /** {@link #scanTag} outcome for a {@code <} that does not start a tag. */
  private static final TagScan LITERAL = new TagScan(null, -1);

  private static final String COMMENT_OPEN = "<!--";
  private static final String CDATA_OPEN = "<![CDATA[";

  /** @param fragment the MessageML to tokenize, never {@code null} */
  public ScanResult scan(String fragment) {
    Objects.requireNonNull(fragment, "fragment");
    List<Token> tokens = new ArrayList<>();
    int n = fragment.length();
    int i = 0;
    // Start of the text run being accumulated (-1 when there is none). A run only ends at real
    // markup, so a literal '<' joins the text around it rather than splitting it.
    int textStart = -1;
    while (i < n) {
      if (fragment.charAt(i) != '<') {
        if (textStart < 0) {
          textStart = i;
        }
        int nextLt = fragment.indexOf('<', i);
        i = nextLt < 0 ? n : nextLt;
        continue;
      }
      if (fragment.startsWith(COMMENT_OPEN, i) || fragment.startsWith(CDATA_OPEN, i)) {
        boolean comment = fragment.startsWith(COMMENT_OPEN, i);
        String terminator = comment ? "-->" : "]]>";
        int close = fragment.indexOf(terminator, i + (comment ? COMMENT_OPEN : CDATA_OPEN).length());
        flushText(tokens, textStart, i);
        textStart = -1;
        if (close < 0) {
          return new ScanResult(tokens, i, true);
        }
        int end = close + terminator.length();
        tokens.add(comment ? new Comment(i, end) : new Cdata(i, end));
        i = end;
        continue;
      }
      TagScan tagScan = isPendingMarkup(fragment, i) ? null : scanTag(fragment, i);
      if (tagScan == LITERAL) {
        if (textStart < 0) {
          textStart = i;
        }
        i++;
        continue;
      }
      flushText(tokens, textStart, i);
      textStart = -1;
      if (tagScan == null) {
        return new ScanResult(tokens, i, true);
      }
      tokens.add(tagScan.token());
      i = tagScan.end();
    }
    flushText(tokens, textStart, n);
    return new ScanResult(tokens, n, false);
  }

  private static void flushText(List<Token> tokens, int start, int end) {
    if (start >= 0 && end > start) {
      tokens.add(new Text(start, end));
    }
  }

  /**
   * A {@code <} at the very end of the fragment that could still turn into markup once the stream
   * continues: a lone {@code <} or {@code </} or a proper prefix of a comment or CDATA opener.
   */
  private static boolean isPendingMarkup(String fragment, int start) {
    // Every pending prefix is shorter than the longest opener, so almost every '<' leaves here
    // without copying the rest of the fragment (this runs once per tag on every streaming update).
    if (fragment.length() - start >= CDATA_OPEN.length()) {
      return false;
    }
    String rest = fragment.substring(start);
    return rest.equals("<") || rest.equals("</")
        || (rest.length() < COMMENT_OPEN.length() && COMMENT_OPEN.startsWith(rest))
        || (rest.length() < CDATA_OPEN.length() && CDATA_OPEN.startsWith(rest));
  }

  /**
   * @return the parsed tag, {@link #LITERAL} when the {@code <} cannot start a tag or {@code null}
   *     when the tag is not finished yet (no terminating {@code >} before the end of the fragment)
   */
  private static TagScan scanTag(String fragment, int start) {
    int n = fragment.length();
    boolean closing = start + 1 < n && fragment.charAt(start + 1) == '/';
    int nameStart = closing ? start + 2 : start + 1;
    int j = nameStart;
    if (j < n && isNameStart(fragment.charAt(j))) {
      j++;
      while (j < n && isNameChar(fragment.charAt(j))) {
        j++;
      }
    }
    if (j == nameStart) {
      return LITERAL;
    }
    String name = fragment.substring(nameStart, j);

    List<int[]> attributeValueRanges = new ArrayList<>();
    char quote = 0;
    int valueStart = -1;
    // An '=' was seen and its value has not started yet, then the start of an unquoted value (-1
    // outside one): a '/' that ends such a value (href=https://x.com/) is part of it, not a "/>".
    boolean valueExpected = false;
    int unquotedValueStart = -1;
    int k = j;
    while (k < n) {
      char c = fragment.charAt(k);
      if (quote != 0) {
        if (c == quote) {
          attributeValueRanges.add(new int[] {valueStart, k});
          quote = 0;
        } else if (c == '<') {
          // XML does not allow a raw '<' in an attribute value, so the quote was never closed
          // (<img src="x > <p>text</p>): what follows is markup or prose, not more of this tag.
          return LITERAL;
        }
      } else if (c == '<') {
        // No tag can contain an unquoted '<': this was prose ("if a<b then c</p>"), not a tag.
        return LITERAL;
      } else if (c == '>') {
        boolean selfClosing = !closing && k > j && fragment.charAt(k - 1) == '/' && unquotedValueStart < 0;
        int end = k + 1;
        Token token = closing
            ? new CloseTag(name, start, end)
            : new OpenTag(name, selfClosing, start, end, List.copyOf(attributeValueRanges));
        return new TagScan(token, end);
      } else if (unquotedValueStart >= 0) {
        if (Character.isWhitespace(c)) {
          unquotedValueStart = -1;
        }
      } else if (c == '"' || c == '\'') {
        quote = c;
        valueStart = k + 1;
        valueExpected = false;
      } else if (c == '=') {
        valueExpected = true;
      } else if (valueExpected && !Character.isWhitespace(c)) {
        unquotedValueStart = k;
        valueExpected = false;
      }
      k++;
    }
    return null;
  }

  private static boolean isNameStart(char c) {
    return Character.isLetter(c) || c == '#' || c == '@';
  }

  private static boolean isNameChar(char c) {
    return Character.isLetterOrDigit(c) || c == '-' || c == ':' || c == '_';
  }
}
