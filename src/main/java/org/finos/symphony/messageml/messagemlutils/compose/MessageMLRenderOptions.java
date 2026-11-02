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

/**
 * Per-request rendering configuration for {@link MarkdownToMessageMLConverter}.
 *
 * <p>The converter is shared as a singleton across the reactive delivery path, so configuration
 * cannot live on the instance. This record is immutable and passed per call instead.
 *
 * @param entitySyntax    when {@code true}, the explicit Symphony source forms
 *                        ({@code #[tag]}, {@code $[TICKER]}, bracketed mentions) are recognised.
 *                        Bare-token detection and mention resolution always apply: they protect
 *                        against what a model emits unprompted, which no toggle can prevent
 * @param contentGrouping when {@code true}, {@code :::card} / {@code :::expand} container directives
 *                        are recognised and a Markdown blockquote stops emitting a card. When
 *                        {@code false} (the default for every existing agent) a blockquote keeps
 *                        emitting {@code <card><body>} and a {@code :::} directive is literal text.
 *                        This way no existing agent changes appearance without opting in
 * @param intermediate    when {@code true} this render is a sampled streaming update rather than the
 *                        final message, which forces {@code state="expanded"} on collapsible
 *                        content so a user never watches an empty box fill up
 * @param nativeMessageML when {@code true}, a {@code <messageML>} block goes through the native
 *                        sanitize/balance/validate/repair/degrade pipeline. When
 *                        {@code false} (the default for every existing agent) a block is rendered as
 *                        escaped literal text instead of being silently dropped
 */
public record MessageMLRenderOptions(
    boolean entitySyntax,
    boolean contentGrouping,
    boolean intermediate,
    boolean nativeMessageML) {

  /**
   * What every call site did before options existed: no explicit entity syntax, no container
   * directives, blockquote still a card. Preserves today's behaviour exactly.
   */
  public static final MessageMLRenderOptions LEGACY =
      new MessageMLRenderOptions(false, false, false, false);

  /** Entity syntax and content grouping enabled, for a final (non-streaming) message. Native
   * MessageML is opt-in separately through {@link #withNativeMessageML}. */
  public static MessageMLRenderOptions full() {
    return new MessageMLRenderOptions(true, true, false, false);
  }

  /** The same options, with native MessageML block rendering turned on or off. */
  public MessageMLRenderOptions withNativeMessageML(boolean enabled) {
    return nativeMessageML == enabled
        ? this
        : new MessageMLRenderOptions(entitySyntax, contentGrouping, intermediate, enabled);
  }

  /** The same options, rendering a sampled streaming update. */
  public MessageMLRenderOptions asIntermediate() {
    return intermediate
        ? this
        : new MessageMLRenderOptions(entitySyntax, contentGrouping, true, nativeMessageML);
  }

  /** The same options, rendering the final message. */
  public MessageMLRenderOptions asFinal() {
    return intermediate
        ? new MessageMLRenderOptions(entitySyntax, contentGrouping, false, nativeMessageML)
        : this;
  }
}
