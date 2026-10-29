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

import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.util.NoOpDataProvider;

import java.util.Locale;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Structural validation, deterministic repair and escaped plain-text fallback for MessageML documents.
 *
 * <p>This is the half of the validation pipeline that knows MessageML: which elements exist, what the
 * library's error messages look like and how each fault can be rewritten. It is deliberately free of
 * Spring, Micrometer and the Symphony BDK, so it can sit next to the converter that produced the markup
 * and be depended on from anywhere. Deciding <em>what to do</em> with the outcome - which counter to
 * increment, how to rebuild a BDK message around repaired content - belongs to the delivery layer.
 *
 * <p>Validation uses {@code NoOpDataProvider}: structure only, no directory lookup and no network, so
 * it is deterministic and safe on the streaming path. Mention <em>resolution</em> stays a server-side
 * concern and cannot be checked here - {@code NoOpDataProvider} resolves any email or uid to a
 * synthetic user.
 *
 * <p><b>The entity payload is part of the input.</b> A document carrying
 * {@code <div class="entity" data-entity-id="…"/>} does not validate on its own - messageml-utils
 * rejects it with "no entity data provided" - so the serialized payload that will be sent must be
 * passed alongside the markup. Validating markup in isolation would reject every message
 * that carries an entity payload.
 *
 * <p>Instances are stateless and safe to share across threads.
 */
public class MessageMLValidator {

  /** Reason labels, bounded so a metric dimension built from them cannot explode. */
  public static final String REASON_MISSING_ENTITY_DATA = "missing_entity_data";
  public static final String REASON_INVALID_ATTRIBUTE_VALUE = "invalid_attribute_value";
  public static final String REASON_DISALLOWED_ELEMENT = "disallowed_element";
  public static final String REASON_MALFORMED_MARKUP = "malformed_markup";
  public static final String REASON_OTHER = "other";

  private static final Pattern ROOT_OPEN = Pattern.compile("^\\s*<messageML>");
  private static final Pattern ROOT_CLOSE = Pattern.compile("</messageML>\\s*$");
  private static final Pattern ENTITY_DIV =
      Pattern.compile("<div\\s+class=\"entity\"[^>]*/>|<div\\s+class=\"entity\"[^>]*>.*?</div>", Pattern.DOTALL);
  /** An ampersand that does not already open a character or entity reference. */
  private static final Pattern STRAY_AMPERSAND = Pattern.compile("&(?!#?\\w+;)");
  private static final Pattern DISALLOWED_ATTRIBUTE =
      Pattern.compile("Attribute \"([\\w-]+)\" is not allowed in \"([\\w-]+)\"");
  private static final Pattern NON_ABSOLUTE_HREF =
      Pattern.compile("The attribute \"href\" must contain an absolute URI");
  /** Reported by messageml-utils for an element outside the MessageML element set. */
  private static final Pattern DISALLOWED_ELEMENT =
      Pattern.compile("Invalid MessageML content at element \"([\\w-]+)\"");
  /** Reported by the XML parser for an element that is opened and never closed. */
  private static final Pattern UNTERMINATED_ELEMENT =
      Pattern.compile("The element type \"([\\w-]+)\" must be terminated");
  /** {@code assertParent} violation: the element exists but not where it was put. */
  private static final Pattern WRONG_PARENT =
      Pattern.compile("Element \"([\\w-]+)\" can only be a child of");
  /** Content-model violation on the parent side, e.g. a {@code <div>} inside a {@code <b>}. */
  private static final Pattern NOT_ALLOWED_IN_PARENT =
      Pattern.compile("Element \"([\\w-]+)\" is not allowed in \"([\\w-]+)\"");
  /** A required attribute is missing - {@code state} on {@code <expandable-card>} in practice. */
  private static final Pattern REQUIRED_ATTRIBUTE =
      Pattern.compile("The attribute \"([\\w-]+)\" is required");
  /** A present attribute whose value is outside the permitted set, which the message enumerates. */
  private static final Pattern ATTRIBUTE_VALUE_NOT_ALLOWED = Pattern.compile(
      "Attribute \"([\\w-]+)\" of element \"([\\w-]+)\" can only be one of the following values: \\[([^]]+)]");
  /** Content-grouping wrappers, unwrapped as a last resort so the answer inside them survives. */
  private static final Pattern CARD_ELEMENT = Pattern.compile(
      "<(card|expandable-card)\\b[^>]*>(.*?)</\\1>", Pattern.DOTALL);
  private static final Pattern CARD_HEADER = Pattern.compile("<header\\b[^>]*>(.*?)</header>", Pattern.DOTALL);
  private static final Pattern CARD_BODY = Pattern.compile("<body\\b[^>]*>(.*?)</body>", Pattern.DOTALL);
  /** The four tags that are only legal together, so they are unwrapped together. */
  private static final Set<String> GROUPING_ELEMENTS =
      Set.of("card", "expandable-card", "header", "body");
  private static final Pattern ANCHOR = Pattern.compile("<a\\s+href=\"([^\"]*)\"\\s*>(.*?)</a>", Pattern.DOTALL);
  private static final Pattern ABSOLUTE_URI = Pattern.compile("(?i)[a-z][a-z0-9+.\\-]*:.*");

  /**
   * A repair may expose the next fault - closing one unterminated element reveals the next one out and
   * an answer nested a few levels deep needs a few passes. Bounded so a pathological document cannot
   * spin: from the caller's point of view this is still one repair pass followed by one re-validation.
   */
  private static final int MAX_REPAIR_PASSES = 6;

  /**
   * Validates a document structurally.
   *
   * @param markup     the full MessageML document
   * @param entityJson the serialized entity payload that will be sent with it or {@code null} when the
   *                   document carries no entity div
   * @return {@code null} when the document is valid, otherwise the validator's message
   */
  public String validate(String markup, String entityJson) {
    if (markup == null || markup.isBlank()) {
      return "empty content";
    }
    try {
      new MessageMLContext(new NoOpDataProvider()).parseMessageML(markup, entityJson, null);
      return null;
    } catch (Exception e) {
      return e.getClass().getSimpleName() + ": " + e.getMessage();
    }
  }

  /**
   * Deterministic, LLM-free repair, driven by the validator's own message so nothing is rewritten
   * speculatively. Each pass fixes the reported fault and re-validates; a fault whose repair exposes
   * the next one (an unterminated element inside another) therefore converges instead of needing a
   * second call from the caller.
   *
   * @return the repaired markup or {@code null} when no repair applies
   */
  public String repair(String markup, String entityJson, String error) {
    if (markup == null) {
      return null;
    }
    String current = markup;
    String currentError = error;
    for (int pass = 0; pass < MAX_REPAIR_PASSES; pass++) {
      String next = repairOnce(current, currentError);
      if (next == null || next.equals(current)) {
        break;
      }
      current = next;
      currentError = validate(current, entityJson);
      if (currentError == null) {
        break;
      }
    }
    return current.equals(markup) ? null : current;
  }

  /**
   * One error-directed rewrite. The first two cases are the failure modes the live benchmark run
   * actually produced - an attribute invented by analogy with a sibling element and a stray ampersand
   * in prose such as "P&amp;L". The others cover the structural faults the pipeline is specified to
   * absorb: a non-absolute anchor destination, an element outside the MessageML set and an element
   * that is never closed.
   *
   * @return the rewritten markup or {@code null} when no rewrite applies to this error
   */
  private static String repairOnce(String markup, String error) {
    if (error == null) {
      return null;
    }
    Matcher badValue = ATTRIBUTE_VALUE_NOT_ALLOWED.matcher(error);
    if (badValue.find()) {
      return setAttribute(markup, badValue.group(2), badValue.group(1), firstAllowed(badValue.group(3)));
    }
    Matcher required = REQUIRED_ATTRIBUTE.matcher(error);
    if (required.find() && "state".equals(required.group(1))) {
      // The one required attribute the grouping elements have. "collapsed" is the authoring default,
      // so a card that lost its state comes back looking the way the model most likely meant.
      return setAttribute(markup, "expandable-card", "state", "collapsed");
    }
    Matcher disallowedAttribute = DISALLOWED_ATTRIBUTE.matcher(error);
    if (disallowedAttribute.find()) {
      return dropAttribute(markup, disallowedAttribute.group(2), disallowedAttribute.group(1));
    }
    if (NON_ABSOLUTE_HREF.matcher(error).find()) {
      return unwrapRelativeAnchors(markup);
    }
    Matcher wrongParent = WRONG_PARENT.matcher(error);
    if (wrongParent.find()) {
      return unwrapMisplaced(markup, wrongParent.group(1));
    }
    Matcher notAllowedInParent = NOT_ALLOWED_IN_PARENT.matcher(error);
    if (notAllowedInParent.find()) {
      return unwrapMisplaced(markup, notAllowedInParent.group(1));
    }
    Matcher disallowedElement = DISALLOWED_ELEMENT.matcher(error);
    if (disallowedElement.find()) {
      return unwrapMisplaced(markup, disallowedElement.group(1));
    }
    Matcher unterminated = UNTERMINATED_ELEMENT.matcher(error);
    if (unterminated.find()) {
      return closeElement(markup, unterminated.group(1));
    }
    String escaped = STRAY_AMPERSAND.matcher(markup).replaceAll("&amp;");
    if (!escaped.equals(markup)) {
      return escaped;
    }
    // Last resort before the fallback: a content-grouping wrapper that cannot be fixed is removed and
    // its contents kept. Losing the box is a presentation loss; losing the answer inside it is not.
    return unwrapCards(markup);
  }

  /** Removes one attribute from every occurrence of one element, leaving the element in place. */
  private static String dropAttribute(String markup, String element, String attribute) {
    Pattern openTag = Pattern.compile("<" + Pattern.quote(element) + "\\b[^>]*>");
    Pattern attributePattern = Pattern.compile("\\s+" + Pattern.quote(attribute) + "=\"[^\"]*\"");

    Matcher matcher = openTag.matcher(markup);
    StringBuilder out = new StringBuilder();
    while (matcher.find()) {
      String cleaned = attributePattern.matcher(matcher.group()).replaceAll("");
      matcher.appendReplacement(out, Matcher.quoteReplacement(cleaned));
    }
    matcher.appendTail(out);
    return out.toString();
  }

  /**
   * Sets one attribute to a known-good value on every occurrence of one element, adding it when absent.
   */
  private static String setAttribute(String markup, String element, String attribute, String value) {
    if (value == null) {
      return null;
    }
    Pattern openTag = Pattern.compile("<" + Pattern.quote(element) + "\\b[^>]*>");
    Pattern existing = Pattern.compile("\\s+" + Pattern.quote(attribute) + "=\"[^\"]*\"");
    String replacement = " " + attribute + "=\"" + value + "\"";

    Matcher matcher = openTag.matcher(markup);
    StringBuilder out = new StringBuilder();
    while (matcher.find()) {
      String tag = matcher.group();
      String cleaned = existing.matcher(tag).replaceAll("");
      // Insert before the closing '>' (or '/>' for a self-closing tag).
      int insertAt = cleaned.endsWith("/>") ? cleaned.length() - 2 : cleaned.length() - 1;
      String rebuilt = cleaned.substring(0, insertAt) + replacement + cleaned.substring(insertAt);
      matcher.appendReplacement(out, Matcher.quoteReplacement(rebuilt));
    }
    matcher.appendTail(out);
    return out.toString();
  }

  /** The validator enumerates the permitted values; the first is the safe one to pick. */
  private static String firstAllowed(String enumeration) {
    String first = enumeration.split(",", 2)[0].trim();
    return first.isEmpty() ? null : first;
  }

  /**
   * Removes {@code <card>} / {@code <expandable-card>} wrappers, keeping the header as emphasised text
   * and the body content as it was. Used only when nothing more targeted applies: the grouping is a
   * presentation choice, the words inside it are the answer.
   */
  private static String unwrapCards(String markup) {
    Matcher matcher = CARD_ELEMENT.matcher(markup);
    StringBuilder out = new StringBuilder();
    boolean changed = false;
    while (matcher.find()) {
      String inner = matcher.group(2);
      Matcher header = CARD_HEADER.matcher(inner);
      Matcher body = CARD_BODY.matcher(inner);
      StringBuilder unwrapped = new StringBuilder();
      if (header.find()) {
        unwrapped.append("<b>").append(header.group(1)).append("</b>");
      }
      unwrapped.append(body.find() ? body.group(1) : stripGroupingTags(inner));
      matcher.appendReplacement(out, Matcher.quoteReplacement(unwrapped.toString()));
      changed = true;
    }
    matcher.appendTail(out);
    if (changed) {
      return out.toString();
    }
    // No balanced wrapper to unwrap: a grouping tag is loose on its own, so drop the tags and keep the
    // text. A stray <card> left behind would fail the next pass just as surely as the header did.
    String stripped = stripGroupingTags(markup);
    return stripped.equals(markup) ? null : stripped;
  }

  private static String stripGroupingTags(String markup) {
    return markup.replaceAll("</?(card|expandable-card|header|body)\\b[^>]*>", "");
  }

  /**
   * Replaces an anchor whose destination is not an absolute URI with its own link text, so the sentence
   * survives without the unusable link. This covers markup the converter did not author - * {@link MarkdownToMessageMLConverter} degrades such a destination at the source.
   */
  private static String unwrapRelativeAnchors(String markup) {
    Matcher matcher = ANCHOR.matcher(markup);
    StringBuilder out = new StringBuilder();
    while (matcher.find()) {
      String replacement = isAbsoluteUri(matcher.group(1)) ? matcher.group() : matcher.group(2);
      matcher.appendReplacement(out, Matcher.quoteReplacement(replacement));
    }
    matcher.appendTail(out);
    return out.toString();
  }

  private static boolean isAbsoluteUri(String destination) {
    return destination != null && ABSOLUTE_URI.matcher(destination).matches();
  }

  /**
   * Unwraps an element that is not where it may be.
   *
   * <p>A content-grouping element is unwrapped as a whole rather than tag by tag: stripping only the
   * {@code <card>} tags would leave {@code <header>} and {@code <body>} orphaned and those are legal
   * <em>only</em> inside a card, so the next pass would fail on them instead. The four grouping tags
   * stand or fall together.
   */
  private static String unwrapMisplaced(String markup, String element) {
    if (GROUPING_ELEMENTS.contains(element)) {
      return unwrapCards(markup);
    }
    return unwrapElement(markup, element);
  }

  /**
   * Removes every tag of one element while keeping its content, so an element MessageML does not know
   * costs its markup and not the words inside it.
   */
  private static String unwrapElement(String markup, String element) {
    return Pattern.compile("</?" + Pattern.quote(element) + "\\b[^>]*>")
        .matcher(markup)
        .replaceAll("");
  }

  /**
   * Closes an element that was opened and never terminated, inserting the end tag just inside the root
   * so the nesting order of anything already closed is preserved.
   */
  private static String closeElement(String markup, String element) {
    String endTag = "</" + element + ">";
    Matcher rootClose = ROOT_CLOSE.matcher(markup);
    if (rootClose.find()) {
      return markup.substring(0, rootClose.start()) + endTag + markup.substring(rootClose.start());
    }
    return markup + endTag;
  }

  /**
   * The last resort: the answer as escaped plain text. Any entity div in the original markup is
   * preserved, because dropping it would orphan the entity payload and break the rich clients that
   * render from it - a fallback must degrade the presentation, never the data.
   *
   * @param markup       the document that could not be made valid, read only for its entity div
   * @param fallbackText the answer in plain text
   */
  public String escapedFallback(String markup, String fallbackText) {
    Matcher entityDiv = ENTITY_DIV.matcher(markup == null ? "" : markup);
    String preservedEntity = entityDiv.find() ? entityDiv.group() : "";
    String body = MarkdownToMessageMLConverter.escapeHtml(fallbackText);
    return "<messageML>" + preservedEntity + body + "</messageML>";
  }

  /** Wraps a rootless body in {@code <messageML>}, leaving an already-rooted document alone. */
  public static String ensureRoot(String content) {
    return ROOT_OPEN.matcher(content).find() ? content : "<messageML>" + content + "</messageML>";
  }

  /**
   * Buckets the validator message so a metric label built from it has bounded cardinality and still
   * distinguishes the failure classes the capability spec calls out.
   */
  public static String reasonOf(String error) {
    String lower = error.toLowerCase(Locale.ROOT);
    if (lower.contains("no entity data provided")) {
      return REASON_MISSING_ENTITY_DATA;
    }
    // Attribute first: "Attribute "accent" is not allowed in "expandable-card"" also matches the
    // element phrasing below and it is an attribute fault, not an unknown element.
    if (lower.contains("attribute")) {
      return REASON_INVALID_ATTRIBUTE_VALUE;
    }
    if (lower.contains("invalid messageml content at element") || lower.contains("is not allowed in")) {
      return REASON_DISALLOWED_ELEMENT;
    }
    if (lower.contains("invalid messageml") || lower.contains("delimiter")
        || lower.contains("must be terminated")) {
      return REASON_MALFORMED_MARKUP;
    }
    return REASON_OTHER;
  }
}
