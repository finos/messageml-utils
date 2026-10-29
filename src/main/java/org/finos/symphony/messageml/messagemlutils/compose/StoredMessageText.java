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

import java.util.regex.Pattern;

/**
 * Makes a stored Symphony message readable as text before it re-enters a prompt.
 *
 * <p>A chat memory that reads an agent's own past messages back through
 * {@code PresentationMLParser.getTextContent} gets every tag stripped with nothing inserted in their
 * place. For a card that is not cosmetic - measured against messageml-utils 1.5.0, the card
 *
 * <pre>{@code <card><header>Risk summary</header><body>Exposure is fine.</body></card>}</pre>
 *
 * comes back as the single run {@code "Risk summaryExposure is fine."}. The model reads its own previous
 * answer with two unrelated sentences fused together and the more content grouping is used the worse it
 * gets.
 *
 * <p><b>Why not {@code MessageMLContext.getMarkdown()}</b>? It does
 * separate header from body correctly, so it would work - but it costs a full messageml-utils parse of
 * every stored message and a memory read can cover many of them per prompt build. It also requires
 * each stored message to still be valid MessageML, which is not something a memory read should depend
 * on. Inserting the lost boundaries into the PresentationML is one regex scan and cannot fail.
 */
public final class StoredMessageText {

  /**
   * The PresentationML classes messageml-utils emits for the grouping elements. Matching on the class
   * rather than on any {@code <div>} keeps the separation targeted: an entity div, for one, must not
   * gain a line break.
   */
  private static final Pattern GROUPING_DIV = Pattern.compile(
      "<div class=\"(?:card|expandable-card|cardHeader|cardBody|expandableCardHeader|expandableCardBody)\"");

  private StoredMessageText() {
  }

  /**
   * Inserts a newline before each content-grouping division, so header and body do not concatenate when
   * the tags are stripped.
   *
   * <p>A no-op for a message that carries no grouping, which is every message an agent without content
   * grouping produces - so this costs one regex scan on the memory path and changes nothing else.
   *
   * @param presentationML the stored message markup, may be {@code null}
   * @return the markup with separators inserted or the input unchanged when there is no grouping
   */
  public static String separateGroupedContent(String presentationML) {
    if (presentationML == null || presentationML.isEmpty()) {
      return presentationML;
    }
    return GROUPING_DIV.matcher(presentationML).replaceAll("\n$0");
  }
}
