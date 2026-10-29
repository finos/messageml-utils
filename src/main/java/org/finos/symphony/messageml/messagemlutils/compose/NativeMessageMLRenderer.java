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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Per-block pipeline for a native {@code <messageML>} block: sanitize, balance, validate, repair,
 * validate again, inline on success or degrade on failure.
 *
 * <p>Repair runs in two stages. {@link MessageMLStructureRepairer} first rebuilds a well-formed
 * fragment out of mismatched or misnested tags, HTML-isms and unescaped text - the faults a model
 * actually makes and the strict {@link MessageMLBalancer} is specified to refuse. Whatever still
 * fails validation then goes through {@link MessageMLValidator#repair}, which fixes the reported
 * attribute or element. Only a block that survives neither is degraded.
 *
 * <p>Each block is handled on its own. The outcome of one block never affects another: the visitor
 * ({@link MarkdownToMessageMLConverter}) calls {@link #render} once per {@code MessageMLBlock} and
 * appends whatever comes back - inlined MessageML, the streaming placeholder or the degraded card -
 * directly to its output, with no further escaping.
 *
 * <p>Stateless and safe to share across threads.
 */
public final class NativeMessageMLRenderer {

  /** Caption shown while a block is being generated, never on the final message. */
  static final String PLACEHOLDER = "<i>✨ Generating rich content…</i>";

  private static final String DEGRADED_HEADER = "⚠️ Rich content could not be displayed";

  private static final Pattern EXPANDABLE_CARD_OPEN_TAG = Pattern.compile("<expandable-card\\b[^>]*>");
  private static final Pattern COLLAPSED_OR_CROPPED_STATE = Pattern.compile("state=\"(collapsed|cropped)\"");

  private static final String ROOT_OPEN = "<messageML>";
  private static final String ROOT_CLOSE = "</messageML>";

  private final MessageMLSanitizer sanitizer = new MessageMLSanitizer();
  private final MessageMLBalancer balancer = new MessageMLBalancer();
  private final MessageMLStructureRepairer structureRepairer = new MessageMLStructureRepairer();
  private final MessageMLValidator validator = new MessageMLValidator();

  /** How a block's content ended up being represented in the output. */
  public enum Outcome {
    /** Rendered exactly as written (after auto-close), no repair needed. */
    RENDERED,
    /** Rendered after the structural or the error-directed repair fixed a fault. */
    REPAIRED,
    /** Replaced by the streaming placeholder or the degraded card - see {@link Result#markup()}. */
    DEGRADED
  }

  /**
   * @param markup  the MessageML fragment to inline into the visitor's output (never escaped again by
   *                the caller) or the placeholder / degraded card when {@link #outcome} is
   *                {@link Outcome#DEGRADED}
   * @param outcome what happened to the block
   */
  public record Result(String markup, Outcome outcome) {}

  /**
   * Runs one block through the pipeline.
   *
   * @param rawBody the block's verbatim source, exactly as the model wrote it (no sanitizing, no
   *                balancing) - kept so a degraded block can still show the user what was written
   * @param options per-request rendering configuration; only {@link MessageMLRenderOptions#intermediate()}
   *                matters here
   * @return the markup to inline and the outcome that produced it
   */
  public Result render(String rawBody, MessageMLRenderOptions options) {
    String sanitized = sanitizer.sanitize(rawBody);
    if (sanitized == null) {
      return degrade(rawBody, options);
    }

    // Fast path: the block is well-formed, at most cut short. Most blocks stop here.
    String balanced = balancer.balance(sanitized);
    String candidate = null;
    String error = null;
    if (balanced != null) {
      candidate = prepare(balanced, options);
      error = validator.validate(wrap(candidate), null);
      if (error == null) {
        return new Result(candidate, Outcome.RENDERED);
      }
    }

    // Structural repair: mismatched, misnested or HTML-flavoured tags the balancer refuses. Sanitized
    // again because renaming can produce a forbidden element ("<FORM>" becomes "<form>").
    String restructured = sanitizer.sanitize(structureRepairer.repair(sanitized));
    if (restructured == null) {
      return degrade(rawBody, options);
    }
    String restructuredCandidate = prepare(restructured, options);
    if (!restructuredCandidate.equals(candidate)) {
      candidate = restructuredCandidate;
      error = validator.validate(wrap(candidate), null);
      if (error == null) {
        return new Result(candidate, Outcome.REPAIRED);
      }
    }

    // Error-directed repair for what is left: attributes, attribute values, unknown elements.
    String repaired = validator.repair(wrap(candidate), null, error);
    if (repaired != null && validator.validate(repaired, null) == null) {
      return new Result(stripRoot(repaired), Outcome.REPAIRED);
    }

    return degrade(rawBody, options);
  }

  private static String prepare(String fragment, MessageMLRenderOptions options) {
    return options.intermediate() ? forceExpanded(fragment) : fragment;
  }

  private static String wrap(String fragment) {
    return ROOT_OPEN + fragment + ROOT_CLOSE;
  }

  private static Result degrade(String rawBody, MessageMLRenderOptions options) {
    if (options.intermediate()) {
      return new Result(PLACEHOLDER, Outcome.DEGRADED);
    }
    String card = "<expandable-card state=\"collapsed\"><header>" + DEGRADED_HEADER + "</header>"
        + "<body><code>" + MarkdownToMessageMLConverter.escapeHtml(rawBody) + "</code></body></expandable-card>";
    return new Result(card, Outcome.DEGRADED);
  }

  /**
   * Rewrites {@code state="collapsed"} and {@code state="cropped"} to {@code state="expanded"} on
   * every {@code <expandable-card>} opening tag, the same rule the converter
   * applies to {@code :::expand} - a collapsed card renders as an empty box while streaming.
   */
  private static String forceExpanded(String candidate) {
    Matcher matcher = EXPANDABLE_CARD_OPEN_TAG.matcher(candidate);
    StringBuilder out = new StringBuilder();
    while (matcher.find()) {
      String rewritten = COLLAPSED_OR_CROPPED_STATE.matcher(matcher.group()).replaceAll("state=\"expanded\"");
      matcher.appendReplacement(out, Matcher.quoteReplacement(rewritten));
    }
    matcher.appendTail(out);
    return out.toString();
  }

  /**
   * Strips the {@code <messageML>}/{@code </messageML>} wrapper repair validated against: the visitor
   * appends into an existing body and must not receive a nested root.
   */
  private static String stripRoot(String markup) {
    String body = markup;
    if (body.startsWith(ROOT_OPEN)) {
      body = body.substring(ROOT_OPEN.length());
    }
    if (body.endsWith(ROOT_CLOSE)) {
      body = body.substring(0, body.length() - ROOT_CLOSE.length());
    }
    return body;
  }
}
