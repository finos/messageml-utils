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
import org.commonmark.node.Document;
import org.commonmark.parser.SourceLine;
import org.commonmark.parser.block.AbstractBlockParser;
import org.commonmark.parser.block.BlockContinue;
import org.commonmark.parser.block.BlockParserFactory;
import org.commonmark.parser.block.BlockStart;
import org.commonmark.parser.block.MatchedBlockParser;
import org.commonmark.parser.block.ParserState;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses a top-level {@code <messageML>...</messageML>} region as a commonmark block.
 *
 * <p>Lines are collected verbatim, blank lines included, until a line containing {@code </messageML>}
 * closes the block or the document ends - the same truncation safety {@link ContainerDirectiveBlockParser}
 * gives {@code :::} directives. Content on the opening and closing lines is kept as body, unless it is
 * only whitespace. Whitespace is tolerated inside the tags ({@code <messageML >}) so a model or a
 * custom template that pads them still opens and closes a block. Whatever follows the closing tag on
 * its line is not part of the block: it is kept as {@link MessageMLBlock#trailingText()} so the
 * converter can render it as Markdown instead of losing it.
 *
 * <p>Only matched at the top level of the document (not inside a list item or a quote) and outside code:
 * the indent check below is the same four-space rule commonmark applies everywhere else and the parent
 * check in {@link Factory} rejects anything matched inside another container block.
 */
class MessageMLBlockParser extends AbstractBlockParser {

  private static final Pattern OPENING_TAG = Pattern.compile("<messageML\\s*>");
  private static final Pattern CLOSING_TAG = Pattern.compile("</messageML\\s*>");

  private final MessageMLBlock block = new MessageMLBlock();

  /**
   * The commonmark engine always calls {@link #addLine} once right after {@code tryStart} opens this
   * block, with whatever is left on the opening line from the index {@code tryStart} returned - empty,
   * since that index is always the end of the line. {@code tryStart} has already recorded any real
   * content from that line on {@link #block} itself, so this first forced call must be discarded or
   * every block would gain a spurious leading blank body line.
   */
  private boolean skipOpeningLineCallback = true;

  @Override
  public Block getBlock() {
    return block;
  }

  @Override
  public BlockContinue tryContinue(ParserState state) {
    if (block.closed()) {
      // Already closed on its own opening line (a single-line block) - nothing more belongs to it.
      return BlockContinue.none();
    }
    String line = state.getLine().getContent().toString();
    Matcher closing = CLOSING_TAG.matcher(line);
    if (closing.find()) {
      closeBlock(block, line, closing);
      return BlockContinue.finished();
    }
    return BlockContinue.atIndex(state.getIndex());
  }

  @Override
  public void addLine(SourceLine line) {
    if (skipOpeningLineCallback) {
      skipOpeningLineCallback = false;
      return;
    }
    block.addBodyLine(line.getContent().toString());
  }

  /** Keeps the text before the closing tag as body and the text after it as trailing Markdown. */
  private static void closeBlock(MessageMLBlock block, String line, Matcher closing) {
    addBodyUnlessBlank(block, line.substring(0, closing.start()));
    block.markClosed(line.substring(closing.end()));
  }

  private static void addBodyUnlessBlank(MessageMLBlock block, String line) {
    if (!line.isBlank()) {
      block.addBodyLine(line);
    }
  }

  static class Factory implements BlockParserFactory {

    @Override
    public BlockStart tryStart(ParserState state, MatchedBlockParser matchedBlockParser) {
      if (state.getIndent() >= 4
          || !(matchedBlockParser.getMatchedBlockParser().getBlock() instanceof Document)) {
        return BlockStart.none();
      }
      String line = state.getLine().getContent().toString().substring(state.getNextNonSpaceIndex());
      Matcher opening = OPENING_TAG.matcher(line);
      if (!opening.lookingAt()) {
        return BlockStart.none();
      }

      MessageMLBlockParser parser = new MessageMLBlockParser();
      String openingLineRemainder = line.substring(opening.end());
      Matcher closing = CLOSING_TAG.matcher(openingLineRemainder);
      if (closing.find()) {
        closeBlock(parser.block, openingLineRemainder, closing);
      } else {
        addBodyUnlessBlank(parser.block, openingLineRemainder);
      }
      return BlockStart.of(parser).atIndex(state.getLine().getContent().length());
    }
  }
}
