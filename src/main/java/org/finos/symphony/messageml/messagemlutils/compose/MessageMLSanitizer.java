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

import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Escapes FreeMarker interpolations and rejects anything in a native MessageML block
 * that must never reach the template engine or create an interactive element.
 *
 * <p>Stateless and safe to share across threads.
 */
public final class MessageMLSanitizer {

  /**
   * The Symphony Elements tag set (forms and their controls), derived from the pinned
   * {@code messageml-utils} 1.5.0 jar rather than retyped from a hand-written list, which
   * the self-correcting derivation test in {@code MessageMLSanitizerTest} caught drifting from the
   * real library in three ways even before any library upgrade: {@code country-code} and
   * {@code dropdown-menu} (both in that list) do not exist as separate elements in 1.5.0 -
   * {@code country-code} is only ever an attribute name ({@code TagAttributes.ATTR_COUNTRYCODE}) and
   * {@code dropdown-menu} is implemented as a plain {@code <select>} - while {@code date-picker} and
   * {@code richtextarea} (both real, separate elements) were missing from it.
   *
   * <p>Ground truth: every non-abstract subclass of
   * {@code org.finos.symphony.messageml.messagemlutils.elements.FormElement} (the library's own base
   * class for a control that must live inside a {@code <form>}, which covers
   * {@code text-field}/{@code textarea}/{@code richtextarea}/{@code select}/{@code option}/
   * {@code checkbox}/{@code radio}/{@code person-selector}/{@code room-selector}/
   * {@code date-picker}/{@code date-selector}/{@code time-picker}/{@code timezone-picker}), plus the
   * four wrapper elements that create or trigger one directly and are not themselves
   * {@code FormElement}s: {@code form}, {@code button}, {@code ui-action} and {@code dialog}. Each
   * tag name is read off the class's own {@code MESSAGEML_TAG} constant, not retyped by hand a second
   * time - see {@code MessageMLSanitizerTest#symphonyElementsAllowlist_isDerivedFromThePinnedMessagemlUtilsJar}.
   */
  static final Set<String> SYMPHONY_ELEMENTS = Set.of(
      "form", "button", "ui-action", "dialog",
      "text-field", "textarea", "richtextarea", "select", "option", "checkbox", "radio",
      "person-selector", "room-selector", "date-picker", "date-selector",
      "time-picker", "timezone-picker");

  /**
   * A FreeMarker directive or macro tag opener anywhere in the fragment - text, attribute value,
   * comment or CDATA alike. FreeMarker runs on the raw string before any XML parsing, so a directive
   * hidden in an attribute value or a comment is evaluated just the same as one written as a tag.
   *
   * <p>messageml-utils 1.5.0 only hands a message to FreeMarker when it contains {@code <#},
   * {@code <@}, {@code ${} or {@code #{} ({@code MessageMLParser.containsFreemarkerTags}). Rejecting
   * the first two and escaping the last two everywhere means none of them survives
   * {@link #sanitize}, so the template engine never runs on a native block at all.
   */
  private static final Pattern FREEMARKER_TAG = Pattern.compile("</?[#@]");

  private static final Pattern ENTITY_CLASS_ATTRIBUTE =
      Pattern.compile("(?i)\\bclass\\s*=\\s*([\"'])\\s*entity\\s*\\1");
  private static final Pattern ENTITY_ID_ATTRIBUTE =
      Pattern.compile("(?i)\\bdata-entity-id\\s*=");

  private final MessageMLTagScanner scanner = new MessageMLTagScanner();

  /**
   * @return the fragment with {@code ${} and {@code #{} escaped or {@code null} when the fragment
   *     must be rejected as a whole
   */
  public String sanitize(String fragment) {
    if (fragment == null || FREEMARKER_TAG.matcher(fragment).find()) {
      return null;
    }
    MessageMLTagScanner.ScanResult scan = scanner.scan(fragment);
    for (MessageMLTagScanner.Token token : scan.tokens()) {
      if (token instanceof MessageMLTagScanner.OpenTag openTag) {
        if (isRejected(openTag.name()) || hasDisallowedAttribute(fragment, openTag)) {
          return null;
        }
      } else if (token instanceof MessageMLTagScanner.CloseTag closeTag) {
        if (isRejected(closeTag.name())) {
          return null;
        }
      }
    }
    // Escaped over the whole string rather than token by token: an interpolation is evaluated
    // wherever it sits, including places no token covers (an unquoted attribute, a comment, CDATA,
    // the unfinished tail of a streaming sample). Nowhere in a MessageML fragment is a literal "${" or
    // "#{" legitimate markup, so there is nothing to preserve.
    return escapeInterpolations(fragment);
  }

  /** Case-insensitive: {@code <FORM>} must not slip past here only to be normalized to a real form. */
  private static boolean isRejected(String tagName) {
    return SYMPHONY_ELEMENTS.contains(tagName.toLowerCase(Locale.ROOT));
  }

  private static boolean hasDisallowedAttribute(String fragment, MessageMLTagScanner.OpenTag tag) {
    String raw = fragment.substring(tag.start(), tag.end());
    return ENTITY_CLASS_ATTRIBUTE.matcher(raw).find() || ENTITY_ID_ATTRIBUTE.matcher(raw).find();
  }

  /** The same {@code ${}/{@code #{} mapping as {@link MarkdownToMessageMLConverter#escapeHtml}. */
  private static String escapeInterpolations(String text) {
    return text.replace("${", "&#36;{").replace("#{", "&#35;{");
  }
}
