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

import org.commonmark.Extension;
import org.commonmark.parser.Parser;

/**
 * Registers the Symphony source-syntax parsers as a commonmark {@link Extension}: the entity sigils as
 * an inline content parser and the {@code :::} container directives as a block parser.
 *
 * <p>Both are always registered. The per-agent toggles in {@link MessageMLRenderOptions} are applied at
 * render time, not parse time, which keeps a single thread-safe {@link Parser} for every agent and every
 * request. The nodes carry their own source text, so a form the agent has not enabled comes back out as
 * the characters the model wrote.
 */
public class SymphonyMarkdownExtension implements Parser.ParserExtension {

  public static Extension create() {
    return new SymphonyMarkdownExtension();
  }

  @Override
  public void extend(Parser.Builder parserBuilder) {
    parserBuilder.customInlineContentParserFactory(new SymphonyEntityInlineParser.Factory());
    parserBuilder.customBlockParserFactory(new ContainerDirectiveBlockParser.Factory());
    parserBuilder.customBlockParserFactory(new MessageMLBlockParser.Factory());
  }
}
