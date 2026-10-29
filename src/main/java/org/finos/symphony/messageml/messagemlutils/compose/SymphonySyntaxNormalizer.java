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

import org.finos.symphony.messageml.messagemlutils.compose.parser.MessageMLBlock;
import org.finos.symphony.messageml.messagemlutils.compose.parser.SymphonyMarkdownExtension;
import org.commonmark.node.AbstractVisitor;
import org.commonmark.node.Code;
import org.commonmark.node.CustomBlock;
import org.commonmark.node.FencedCodeBlock;
import org.commonmark.node.IndentedCodeBlock;
import org.commonmark.node.Node;
import org.commonmark.node.SourceSpan;
import org.commonmark.parser.IncludeSourceSpans;
import org.commonmark.parser.Parser;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Turns Symphony source syntax back into ordinary Markdown.
 *
 * <p>Needed in two places, for the same underlying reason: the extended syntax is a private contract
 * between the producer and the model and anything that reads the model's words rather than the rendered
 * message must not see it.
 *
 * <ul>
 *   <li><b>Raw-text payloads.</b> A payload that carries the raw model output alongside the message
 *       would otherwise hand {@code :::card} and {@code #[topic]} directives verbatim to rich clients
 *       once the model emits them. Normalizing keeps the published text clean Markdown. The
 *       Symphony Chat MessageML body is unaffected - it still gets the full rendering.</li>
 *   <li><b>Chat memory.</b> A stored message is read back through
 *       {@code PresentationMLParser.getTextContent}, which flattens all markup, so a card's header and
 *       body arrive concatenated with no delimiter. Normalizing before the text re-enters a prompt keeps
 *       the separation the model needs to make sense of its own past answer.</li>
 * </ul>
 *
 * <p>A plain {@code mailto:} link keeps its address after the label ({@code Email us (help@x.com)}) unless
 * the label already shows it. A mention written with the {@code @} sigil reduces to the name alone.
 *
 * <p>Code is never rewritten: a fenced block, an indented block or an inline code span that shows the
 * syntax ({@code `#[topic]`}, a {@code :::card} example) comes back exactly as the model wrote it.
 *
 * <p>Stateless and safe to share.
 */
public final class SymphonySyntaxNormalizer {

  private static final Pattern CONTAINER_OPEN = Pattern.compile(
      "(?m)^:::[ \\t]*(card|expandable|expand)(?:\\[[a-zA-Z]+])?[ \\t]*(.*)$");
  private static final Pattern CONTAINER_CLOSE = Pattern.compile("(?m)^:::[ \\t]*$\\R?");
  /** Same value forms as {@code ContainerDirectiveBlockParser}: double-quoted, single-quoted or bare. */
  private static final Pattern ATTRIBUTE = Pattern.compile("^[a-zA-Z]+=(?:\"[^\"]*\"|'[^']*'|\\S+)\\s*");
  private static final Pattern LINE_BREAK = Pattern.compile("\\r\\n?");
  private static final Pattern EXPLICIT_TAG = Pattern.compile("[#$]\\[([^\\[\\]\\s]+)]");
  private static final Pattern BRACKET_MENTION = Pattern.compile("@\\[([^\\[\\]]+)]\\(([^()\\s]+)\\)");
  private static final Pattern MAILTO_LINK = Pattern.compile("\\[([^\\[\\]]+)]\\(mailto:([^()\\s]+)\\)");
  private static final Pattern SYM_USER_MENTION = Pattern.compile("\\[([^\\[\\]]+)]\\(sym:user/(\\d+)\\)");
  private static final Pattern BLANK_RUN = Pattern.compile("\\n{3,}");
  private static final String RICH_CONTENT_MARKER = "[rich content]";
  private static final String NATIVE_BLOCK_OPEN = "<messageML";

  /**
   * Stands in for a piece of code while the passes run: private-use characters around its position on
   * the shelf, which no pass reads as syntax. Text that already holds one is not shelved at all.
   */
  private static final char SHELF_OPEN = '\uE000';
  private static final char SHELF_CLOSE = '\uE001';

  /**
   * Parses only enough to find {@code <messageML>} blocks and code: no tables extension, no
   * rendering, just source spans on the same {@link MessageMLBlock} node the converter itself uses.
   * A regex cannot tell a fenced example apart from a real top-level block or code from prose the way
   * a real block parser already does, so both are located with the parser.
   */
  private static final Parser NATIVE_BLOCK_PARSER = Parser.builder()
      .extensions(List.of(SymphonyMarkdownExtension.create()))
      .includeSourceSpans(IncludeSourceSpans.BLOCKS_AND_INLINES)
      .build();

  /** A stretch of the input the parser found: a native block to replace or a piece of code to keep as is. */
  private record Region(int start, int end, boolean code, String trailingText) {}

  private SymphonySyntaxNormalizer() {
  }

  /**
   * Resolves container directives to plain Markdown, replaces native MessageML blocks with a marker
   * and reduces the entity forms to their display text.
   *
   * <p>A directive title becomes a bold line rather than a heading: it is a label inside a message, not
   * a document section and a heading would outrank the headings the model wrote itself.
   *
   * @param markdown raw model output, may be {@code null}
   * @return clean Markdown, never {@code null}
   */
  public static String toPlainMarkdown(String markdown) {
    if (markdown == null || markdown.isEmpty()) {
      return markdown == null ? "" : markdown;
    }
    String result = markdown.indexOf('\r') < 0 ? markdown : LINE_BREAK.matcher(markdown).replaceAll("\n");
    // Null when the text already holds a shelf character: code is then left to the passes as before.
    List<String> shelf = result.indexOf(SHELF_OPEN) < 0 && result.indexOf(SHELF_CLOSE) < 0
        ? new ArrayList<>()
        : null;
    if (result.contains(NATIVE_BLOCK_OPEN) || result.contains(":::") || hasEntityForm(result)) {
      result = shelve(result, shelf);
    }
    String resolved = resolveContainers(result);
    if (!resolved.equals(result) && (resolved.contains(NATIVE_BLOCK_OPEN) || hasEntityForm(resolved))) {
      // A directive body is raw source for the parser, so code or a native block inside one only
      // shows once the directive is gone.
      resolved = shelve(resolved, shelf);
    }
    result = reduceEntities(resolved);
    result = BLANK_RUN.matcher(result).replaceAll("\n\n").strip();
    return unshelve(result, shelf);
  }

  /**
   * Splices {@link #RICH_CONTENT_MARKER} over each native block's exact source span and moves each piece
   * of code onto {@code shelf}, leaving a token in its place. Everything else is left untouched,
   * including a block's own raw content, so nothing it contains (a stray {@code :::} or entity sigil)
   * is mistaken for Symphony source syntax by the passes that follow.
   *
   * <p>Most responses hold none of the triggers the caller checks ({@code <messageML}, {@code :::} or an
   * entity form), which keeps them from being parsed into an AST at all.
   *
   * @param shelf where code is put or {@code null} to leave it in place
   */
  private static String shelve(String markdown, List<String> shelf) {
    List<Region> regions = new ArrayList<>();
    NATIVE_BLOCK_PARSER.parse(markdown).accept(new AbstractVisitor() {
      @Override
      public void visit(CustomBlock customBlock) {
        if (customBlock instanceof MessageMLBlock block) {
          // Never empty: IncludeSourceSpans gives every block one span per line it covers.
          regions.add(region(block, false, block.trailingText()));
        } else {
          super.visit(customBlock);
        }
      }

      @Override
      public void visit(FencedCodeBlock fencedCodeBlock) {
        addCode(fencedCodeBlock);
      }

      @Override
      public void visit(IndentedCodeBlock indentedCodeBlock) {
        addCode(indentedCodeBlock);
      }

      @Override
      public void visit(Code code) {
        addCode(code);
      }

      private void addCode(Node code) {
        if (shelf != null && !code.getSourceSpans().isEmpty()) {
          regions.add(region(code, true, ""));
        }
      }
    });
    if (regions.isEmpty()) {
      return markdown;
    }
    StringBuilder out = new StringBuilder();
    int cursor = 0;
    for (Region region : regions) {
      if (region.start() < cursor) {
        continue;
      }
      out.append(markdown, cursor, region.start());
      if (region.code()) {
        shelf.add(markdown.substring(region.start(), region.end()));
        out.append(SHELF_OPEN).append(shelf.size() - 1).append(SHELF_CLOSE);
      } else {
        out.append(RICH_CONTENT_MARKER);
        if (!region.trailingText().isEmpty()) {
          // Words after the closing tag sit inside the block's last source line but are not the block.
          out.append('\n').append(region.trailingText());
        }
      }
      cursor = region.end();
    }
    return out.append(markdown, cursor, markdown.length()).toString();
  }

  private static Region region(Node node, boolean code, String trailingText) {
    List<SourceSpan> spans = node.getSourceSpans();
    SourceSpan first = spans.get(0);
    SourceSpan last = spans.get(spans.size() - 1);
    return new Region(first.getInputIndex(), last.getInputIndex() + last.getLength(), code, trailingText);
  }

  /** Puts the shelved code back, last in first out: code found on a second pass can hold an earlier token. */
  private static String unshelve(String text, List<String> shelf) {
    if (shelf == null || shelf.isEmpty()) {
      return text;
    }
    String result = text;
    for (int i = shelf.size() - 1; i >= 0; i--) {
      result = result.replace("" + SHELF_OPEN + i + SHELF_CLOSE, shelf.get(i));
    }
    return result;
  }

  private static String resolveContainers(String markdown) {
    if (!markdown.contains(":::")) {
      return markdown;
    }
    Matcher opening = CONTAINER_OPEN.matcher(markdown);
    StringBuilder out = new StringBuilder();
    while (opening.find()) {
      String title = stripAttributes(opening.group(2));
      // A directive with no title leaves nothing behind: an empty bold line would read as a glitch.
      // The matched region excludes the line's own newline, so no separator is added here.
      String replacement = title.isEmpty() ? "" : "**" + title + "**";
      opening.appendReplacement(out, Matcher.quoteReplacement(replacement));
    }
    opening.appendTail(out);
    return CONTAINER_CLOSE.matcher(out.toString()).replaceAll("");
  }

  /** Attribute pairs on the opening line are rendering hints, not words the reader wants. */
  private static String stripAttributes(String rest) {
    String remaining = rest == null ? "" : rest.trim();
    while (true) {
      Matcher attribute = ATTRIBUTE.matcher(remaining);
      if (!attribute.find()) {
        return remaining;
      }
      remaining = remaining.substring(attribute.end());
    }
  }

  /** What every entity form holds: a sigil and its bracket or the destination that makes a link a mention. */
  private static boolean hasEntityForm(String markdown) {
    return markdown.contains("#[") || markdown.contains("$[") || markdown.contains("@[")
        || markdown.contains("](mailto:") || markdown.contains("](sym:user/");
  }

  private static String reduceEntities(String markdown) {
    if (!hasEntityForm(markdown)) {
      return markdown;
    }
    String result = BRACKET_MENTION.matcher(markdown).replaceAll("$1");
    result = SYM_USER_MENTION.matcher(result).replaceAll("$1");
    result = keepMailtoAddresses(result);
    return EXPLICIT_TAG.matcher(result).replaceAll("$1");
  }

  /**
   * A plain {@code [label](mailto:address)} link, without the {@code @} sigil, is not known to be a person:
   * {@code [Email us](mailto:help@x.com)} must not lose the address. It follows the label unless the label
   * already shows it. The sigil form is a mention and reduces to the name alone.
   */
  private static String keepMailtoAddresses(String markdown) {
    Matcher link = MAILTO_LINK.matcher(markdown);
    StringBuilder out = new StringBuilder();
    while (link.find()) {
      String label = link.group(1);
      String address = link.group(2);
      int query = address.indexOf('?');
      if (query >= 0) {
        address = address.substring(0, query);
      }
      boolean shown = address.isEmpty()
          || label.toLowerCase(Locale.ROOT).contains(address.toLowerCase(Locale.ROOT));
      link.appendReplacement(out, Matcher.quoteReplacement(shown ? label : label + " (" + address + ")"));
    }
    return link.appendTail(out).toString();
  }
}
