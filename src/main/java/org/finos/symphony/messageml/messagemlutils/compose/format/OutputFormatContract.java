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

package org.finos.symphony.messageml.messagemlutils.compose.format;

/**
 * The {@code [OUTPUT FORMAT]} block that teaches a model the Symphony source syntax.
 *
 * <p><b>The wording is a measured constant, not copy.</b> Negative cases are written as explicit
 * wrong/right pairs and the safe default comes first, where it frames everything after it. Do not
 * edit it in passing.
 *
 * <p>Three invariants, all asserted by the accompanying test:
 *
 * <ul>
 *   <li><b>No {@code #{}.</b> The block survives expression-template engines that evaluate
 *       {@code #{...}}. This is a concrete reason the {@code :::} and {@code #[…]} forms were chosen
 *       over a {@code {{…}}} style.</li>
 *   <li><b>No stray {@code %}.</b> The block survives {@code String.format}.</li>
 *   <li><b>Never names MessageML, HTML or XML.</b> Naming the output dialect invites the model to emit
 *       tags; it should believe it is writing Markdown, because it is. This invariant applies to
 *       {@link #block()} / {@link #block(boolean) block(false)} only: the native-block section added by
 *       {@link #block(boolean) block(true)} exists specifically to teach the model that one dialect by
 *       name.</li>
 * </ul>
 */
public final class OutputFormatContract {

  private static final String BLOCK = """
      [OUTPUT FORMAT]
      Reply in Markdown. When in doubt about any special form below, write plain text - plain text
      always renders correctly. Standard Markdown (bold, lists, tables, fenced code) needs no special
      treatment; use it normally.

      Three special forms exist. Each one means "this is the entity itself", not "this text mentions
      an entity". Use them only when that is true.

      Mention a person       write   @[Jane Doe](mailto:jane.doe@example.com)
      Mention by numeric ID  write   @[Jane Doe](sym:user/12345)   when you only have the ID, not the email
      Topic tag             write   #[quarterly-review]
      Financial instrument  write   $[AAPL]

      The mistake to avoid is using a form for something that merely looks like one:

      money amount          write   $12 million        not   $[12] million
      issue or ticket       write   #123               not   #[123]
      version number        write   #2.1               not   #[2.1]
      a plain word          write   the $ sign         not   $[sign]

      Two block forms group content. Use them for substantial content only, never for a short answer,
      and never one inside the other.

      A titled block:
      :::card
      ### Header line
      Body content.
      :::

      Detail the reader must click to reveal:
      :::expand Quarterly breakdown
      Detail here.
      :::

      Links to a Symphony room or message: when a tool result gives a link, copy it exactly as
      written. Otherwise build one from the IDs you were given, copied as-is (the platform fixes
      their encoding). Write the bare link between angle brackets, with no label:

      Room                  write   <https://open.symphony.com/?streamId={streamId}&streamType=chatroom>
      One-to-one chat       write   <https://open.symphony.com/?streamId={streamId}&streamType=im>
      Message               write   <https://open.symphony.com/?streamId={streamId}&messageId={messageId}>

      Never invent an ID. Without the IDs, name the room or message in plain text.
      """;

  /**
   * Appended after {@link #BLOCK} when native blocks are enabled. Short on purpose: the authoring rules
   * live in {@link SymphonyContentFormatGuide}, not here. A caller that exposes the guide through some
   * other channel can append its own pointer after {@link #block(boolean) block(true)}.
   */
  private static final String NATIVE_BLOCK = """

      One more form, for content Markdown cannot express: a styled layout, a card with an icon, a
      coloured span, a structured date. Read the content format guide once before
      your first block to see the full rules and the supported elements, then write:

      <messageML>
      ...MessageML markup here...
      </messageML>

      Use it only when the content genuinely needs it, never for a short answer. Switch back to
      Markdown right after - the two forms can alternate freely within one answer.
      """;

  private OutputFormatContract() {
  }

  /** The base block, byte-identical whatever the native-block flag - the benchmarked wording. */
  public static String block() {
    return BLOCK;
  }

  /** The base block, followed by the native-block section when {@code nativeMessageML} is {@code true}. */
  public static String block(boolean nativeMessageML) {
    return nativeMessageML ? BLOCK + NATIVE_BLOCK : BLOCK;
  }
}
