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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Lenient, deterministic structural repair for a native MessageML block that the strict
 * {@link MessageMLBalancer} rejected or that failed validation: the "repair" leg of the native-block
 * pipeline for faults that are about tag structure rather than about one attribute.
 *
 * <p>Where the balancer refuses anything that is not merely cut short, this class rebuilds a
 * well-formed fragment out of tag soup, the way an HTML parser would, using what messageml-utils
 * 1.5.0 actually accepts as its content model (probed, not assumed):
 *
 * <ul>
 *   <li>A closing tag that matches an element further down the stack closes everything opened above
 *       it ({@code <p><b>world</p>} becomes {@code <p><b>world</b></p>}). A closing tag that matches
 *       nothing open is dropped, unless it is plainly a mistyped close of the inline element on top
 *       ({@code <i>x</b>}).
 *   <li>An opening tag that cannot be a child of what is open implicitly closes it first: a new
 *       {@code <li>}, {@code <tr>}, {@code <td>}/{@code <th>} or table section closes the previous
 *       one, {@code <header>}/{@code <body>} closes whatever is still open inside the card and a
 *       {@code <p>} closes an open paragraph. A cell written straight inside a table gets its
 *       {@code <tr>}, a row or a table section written without its {@code <table>} gets one. Text
 *       written straight inside a list gets its {@code <li>} and text written before the first row of
 *       a table (a caption) gets a row and a cell of its own.
 *   <li>A block element ({@code <p>}, {@code <div>}, a list, a table, a heading, a card...) opened
 *       inside an element that only takes phrasing content ({@code <b>}, {@code <i>}, {@code <code>},
 *       {@code <pre>}, a heading) closes it first. Bold and italic are then reopened around the text
 *       that follows, so {@code <b>a<p>b</p>c</b>} keeps all three words bold.
 *   <li>{@code <br>}, {@code <hr>} and {@code <img>} are self-closed. An empty list, which
 *       MessageML rejects, is removed.
 *   <li>Text is made XML-safe: a raw {@code <} or a stray {@code &} is escaped and an HTML named
 *       reference ({@code &nbsp;}, {@code &mdash;}) becomes a numeric one. CDATA becomes escaped
 *       text and comments, which MessageML rejects, are dropped.
 *   <li>Names are normalized: {@code <B>} is {@code <b>}, {@code <datetime>} is {@code <dateTime>},
 *       {@code <strong>}/{@code <em>} are {@code <b>}/{@code <i>} and {@code <u>}, {@code <s>},
 *       {@code <del>}... become a styled {@code <span>}. A stray nested {@code <messageML>} is
 *       dropped.
 *   <li>Attributes are made well-formed: an unquoted value is quoted and a duplicate attribute is
 *       dropped. A value-less one is dropped too unless it is one of the form booleans
 *       ({@code checked}, {@code selected}, {@code required}...), which become {@code ="true"}. An
 *       emoji written {@code :rocket:} loses its colons.
 *   <li>An unfinished tail is cut, empty elements left open at the end are pruned and the rest is
 *       closed innermost first, exactly as the balancer does, so a streaming sample is handled too.
 * </ul>
 *
 * <p>The output is always well-formed, but not necessarily valid MessageML: attribute values, unknown
 * elements and the like are still {@link MessageMLValidator#repair}'s job. It is not sanitized either
 * (a renamed tag can become a forbidden one), so the caller sanitizes again before validating.
 * A fragment that is already well-formed MessageML comes back unchanged.
 *
 * <p>Stateless and safe to share across threads.
 */
public final class MessageMLStructureRepairer {

  private static final String TABLE = "table";
  private static final String THEAD = "thead";
  private static final String TBODY = "tbody";
  private static final String TFOOT = "tfoot";
  private static final String HEADER = "header";
  private static final String EXPANDABLE_CARD = "expandable-card";
  private static final String SHORTCODE = "shortcode";
  private static final String AMPERSAND_ENTITY = "&amp;";

  private static final Set<String> VOID_ELEMENTS = Set.of("br", "hr", "img");

  private static final Set<String> HEADINGS = Set.of("h1", "h2", "h3", "h4", "h5", "h6");

  /** Elements messageml-utils only lets hold phrasing content. */
  private static final Set<String> PHRASING_ONLY = union(Set.of("b", "i", "code", "pre"), HEADINGS);

  /** Elements messageml-utils refuses inside a {@link #PHRASING_ONLY} parent. */
  private static final Set<String> BLOCKS =
      union(Set.of("p", "div", "ul", "ol", TABLE, "pre", "code", "card", EXPANDABLE_CARD), HEADINGS);

  /** Formatting that is reopened inside a block that interrupted it. */
  private static final Set<String> FORMATTING = Set.of("b", "i");

  /** The phrasing elements a new {@code <p>} looks through for a paragraph left open. */
  private static final Set<String> INLINE = Set.of("b", "i", "span", "a", "code", "sub", "sup");

  /** Containers that hold no text or phrasing content of their own. */
  private static final Set<String> NO_PHRASING = Set.of("ul", "ol", TABLE, THEAD, TBODY, TFOOT, "tr");

  private static final Set<String> STRUCTURAL =
      Set.of("li", "tr", "td", "th", THEAD, TBODY, TFOOT, HEADER, "body");

  private static final Set<String> LISTS = Set.of("ul", "ol");
  private static final Set<String> TABLE_SECTIONS = Set.of(THEAD, TBODY, TFOOT);
  private static final Set<String> TABLE_CONTAINERS = Set.of(TABLE, THEAD, TBODY, TFOOT);
  private static final Set<String> CELLS = Set.of("td", "th");
  /** What only makes sense inside a {@code <table>}. */
  private static final Set<String> TABLE_PARTS = union(union(TABLE_SECTIONS, CELLS), Set.of("tr"));
  private static final Set<String> CARDS = Set.of("card", EXPANDABLE_CARD);
  private static final Set<String> CARD_PARTS = Set.of(HEADER, "body");

  /** Element families whose members a model mixes up when closing: {@code <ul>...</ol>}, {@code <h2>...</h3>}. */
  private static final List<Set<String>> FAMILIES = List.of(LISTS, CELLS, TABLE_SECTIONS, HEADINGS);

  /** Where a new {@code <li>} stops looking for the previous one. */
  private static final Set<String> LIST_ITEM_SCOPE =
      Set.of("ul", "ol", TABLE, "td", "th", "card", EXPANDABLE_CARD, HEADER, "body");

  private static final String ROOT = "messageML";

  /** The two MessageML element names that are not all lower case. */
  private static final Map<String, String> CAMEL_CASE_NAMES = Map.of("datetime", "dateTime", "messageml", ROOT);

  private record Alias(String name, String style) {}

  private static final String UNDERLINE = "text-decoration: underline;";
  private static final String LINE_THROUGH = "text-decoration: line-through;";

  /** HTML elements a model reaches for that MessageML names differently. */
  private static final Map<String, Alias> ALIASES = Map.of(
      "strong", new Alias("b", null),
      "em", new Alias("i", null),
      "u", new Alias("span", UNDERLINE),
      "ins", new Alias("span", UNDERLINE),
      "s", new Alias("span", LINE_THROUGH),
      "strike", new Alias("span", LINE_THROUGH),
      "del", new Alias("span", LINE_THROUGH));

  /** A character or entity reference, matched at an {@code &}. */
  private static final Pattern REFERENCE =
      Pattern.compile("&(?:#[0-9]{1,7}|#[xX][0-9a-fA-F]{1,6}|([A-Za-z][A-Za-z0-9]{0,31}));");

  /** An ampersand in CDATA, except the sanitizer's own {@code &#36;{} / {@code &#35;{} escapes. */
  private static final Pattern CDATA_AMPERSAND = Pattern.compile("&(?!#3[56];\\{)");

  /**
   * The attributes messageml-utils' form elements take as {@code true} or {@code false}. Written the HTML
   * way, with no value, they are not even well-formed XML.
   */
  private static final Set<String> BOOLEAN_ATTRIBUTES =
      Set.of("checked", "selected", "required", "multiple", "masked", "disabled", "readonly", "auto-submit", "strict");

  private static final Pattern EMOJI_COLONS = Pattern.compile("^:([^:]+):$");
  /** What messageml-utils rejects in a shortcode ("thumbs up!"), replaced by an underscore. */
  private static final Pattern EMOJI_INVALID = Pattern.compile("[^A-Za-z0-9_+\\-]+");

  /**
   * Elements that mean nothing without one of these attributes. One written without them is unwrapped
   * (an {@code <a>} keeps its text) instead of failing the whole block on "The attribute ... is
   * required", a message that does not even say which element it is about.
   */
  private static final Map<String, List<String>> REQUIRED_ATTRIBUTES = Map.of(
      "a", List.of("href"),
      "img", List.of("src"),
      "emoji", List.of(SHORTCODE, "annotation"),
      "hash", List.of("tag"),
      "cash", List.of("tag"),
      "mention", List.of("uid", "email"),
      "dateTime", List.of("value"));

  private final MessageMLTagScanner scanner = new MessageMLTagScanner();

  /**
   * @param fragment a sanitized block body, as given to {@link MessageMLBalancer#balance}
   * @return a well-formed fragment, never {@code null} for a non-null input
   */
  public String repair(String fragment) {
    return fragment == null ? null : new Run(fragment, scanner.scan(fragment)).repair();
  }

  private static final class Frame {
    final String name;
    final String openTag;
    final int openSegment;
    /** Reopened by the repair itself, not written by the model. */
    final boolean reconstructed;
    /** Kept on the stack to match its closing tag, but written without any tags (see REQUIRED_ATTRIBUTES). */
    boolean unwrapped;
    boolean hasContent;
    /** For a list: its last closed {@code <li>} and the index of that closing tag in the output. */
    Frame lastItem;
    int lastItemCloseSegment = -1;

    Frame(String name, String openTag, int openSegment, boolean reconstructed) {
      this.name = name;
      this.openTag = openTag;
      this.openSegment = openSegment;
      this.reconstructed = reconstructed;
    }
  }

  /** Bold or italic that a block element interrupted and the model has not closed yet. */
  private static final class Suspended {
    final String name;
    final String openTag;
    /** Stack size with its parent on top, so it is forgotten once that parent closes. */
    final int ownerDepth;
    /** Where it is currently reopened, if anywhere. */
    Frame reopened;

    Suspended(String name, String openTag, int ownerDepth) {
      this.name = name;
      this.openTag = openTag;
      this.ownerDepth = ownerDepth;
    }
  }

  /** One repair: the state of a single left-to-right pass over the tokens. */
  private static final class Run {
    private final String fragment;
    private final MessageMLTagScanner.ScanResult scan;
    private final List<String> out = new ArrayList<>();
    private final Deque<Frame> stack = new ArrayDeque<>();
    private final List<Suspended> suspended = new ArrayList<>();
    /** Names closed implicitly, so the model's own late closing tag for each can be dropped. */
    private final Map<String, Integer> implicitlyClosed = new HashMap<>();

    Run(String fragment, MessageMLTagScanner.ScanResult scan) {
      this.fragment = fragment;
      this.scan = scan;
    }

    String repair() {
      List<MessageMLTagScanner.Token> tokens = scan.tokens();
      for (int t = 0; t < tokens.size(); t++) {
        MessageMLTagScanner.Token token = tokens.get(t);
        if (token instanceof MessageMLTagScanner.OpenTag openTag) {
          MessageMLTagScanner.Token next = t + 1 < tokens.size() ? tokens.get(t + 1) : null;
          if (open(openTag, next)) {
            t++;
          }
        } else if (token instanceof MessageMLTagScanner.CloseTag closeTag) {
          close(canonicalName(closeTag.name()));
        } else if (token instanceof MessageMLTagScanner.Text text) {
          text(normalizeText(source(text)));
        } else if (token instanceof MessageMLTagScanner.Cdata cdata) {
          String content = fragment.substring(cdata.start() + "<![CDATA[".length(), cdata.end() - "]]>".length());
          text(CDATA_AMPERSAND.matcher(content).replaceAll(AMPERSAND_ENTITY).replace("<", "&lt;").replace(">", "&gt;"));
        }
        // Comments are dropped: messageml-utils rejects them ("Invalid element #comment").
      }
      drain();
      return String.join("", out);
    }

    private String source(MessageMLTagScanner.Token token) {
      return fragment.substring(token.start(), token.end());
    }

    /** @return {@code true} when {@code next} was consumed as this void element's own closing tag */
    private boolean open(MessageMLTagScanner.OpenTag tag, MessageMLTagScanner.Token next) {
      String rawName = tag.name();
      String name = canonicalName(rawName);
      if (ROOT.equals(name)) {
        return false;
      }
      Alias alias = ALIASES.get(rawName.toLowerCase(Locale.ROOT));
      String openTag = rebuildOpenTag(tag, name, alias);
      boolean unwrapped = openTag == null;

      closeImpliedBy(name);
      // Implicit parents, for an element whose own opening tag was lost.
      if (TABLE_PARTS.contains(name) && nearest(Set.of(TABLE), Set.of()) == null) {
        push(TABLE, "<table>", false);
      }
      if (CELLS.contains(name) && top() != null && TABLE_CONTAINERS.contains(top().name)) {
        push("tr", "<tr>", false);
      } else if (name.equals("li") && (top() == null || !LISTS.contains(top().name))) {
        push("ul", "<ul>", false);
      } else if (CARD_PARTS.contains(name) && nearest(CARDS, Set.of()) == null) {
        push("card", "<card>", false);
      }
      if (!BLOCKS.contains(name) && !STRUCTURAL.contains(name)) {
        startPhrasing();
      }

      boolean ownCloseFollows = next instanceof MessageMLTagScanner.CloseTag closeTag
          && name.equals(canonicalName(closeTag.name()));
      if (unwrapped && (tag.selfClosing() || VOID_ELEMENTS.contains(name))) {
        return VOID_ELEMENTS.contains(name) && ownCloseFollows;
      }
      if (tag.selfClosing()) {
        out.add(openTag);
        markContent();
        return false;
      }
      if (VOID_ELEMENTS.contains(name)) {
        markContent();
        if (ownCloseFollows) {
          out.add(openTag);
          out.add("</" + name + ">");
          return true;
        }
        out.add(openTag.substring(0, openTag.length() - 1) + "/>");
        return false;
      }
      push(name, unwrapped ? "" : openTag, false).unwrapped = unwrapped;
      return false;
    }

    /** Implicit end tags: closes whatever {@code name} cannot be opened inside. */
    private void closeImpliedBy(String name) {
      if (name.equals("li")) {
        closeThroughNearest(Set.of("li"), LIST_ITEM_SCOPE);
      } else if (name.equals("tr")) {
        closeThroughNearest(Set.of("tr"), TABLE_CONTAINERS);
      } else if (CELLS.contains(name)) {
        closeThroughNearest(CELLS, union(Set.of("tr"), TABLE_CONTAINERS));
      } else if (TABLE_SECTIONS.contains(name)) {
        closeAbove(nearest(Set.of(TABLE), Set.of()));
      } else if (CARD_PARTS.contains(name)) {
        closeAbove(nearest(CARDS, Set.of()));
      } else if (name.equals("p")) {
        Frame paragraph = null;
        for (Frame frame : stack) {
          if (frame.name.equals("p")) {
            paragraph = frame;
            break;
          }
          if (!INLINE.contains(frame.name)) {
            break;
          }
        }
        if (paragraph != null) {
          closeThrough(paragraph);
        }
      }
      if (BLOCKS.contains(name) && top() != null) {
        if (LISTS.contains(name) && LISTS.contains(top().name) && top().lastItem != null) {
          // "<ul><li>a</li><ul>...": a nested list written as a sibling. It goes into the item before
          // it, which is reopened by taking its closing tag back.
          Frame list = top();
          out.set(list.lastItemCloseSegment, "");
          stack.push(list.lastItem);
          list.lastItem = null;
        } else if (LISTS.contains(top().name)) {
          // "<ul><li>a</li><p>...": the list was never closed.
          closeImplicitly();
        } else if (NO_PHRASING.contains(top().name)) {
          // Same for a table: a paragraph cannot sit between its rows.
          closeThroughNearest(Set.of(TABLE), Set.of());
        }
      }
      if (name.equals("code") && top() != null && top().name.equals("pre") && !top().hasContent) {
        // "<pre><code>...</code></pre>", the usual way to write a code block: messageml-utils refuses a
        // code in a pre (the other way round is fine). The pre is dropped and the code keeps the block,
        // along with its language. Closed implicitly right below, without writing a closing tag.
        top().unwrapped = true;
        out.set(top().openSegment, "");
      }
      if (BLOCKS.contains(name)) {
        List<Frame> interrupted = new ArrayList<>();
        while (top() != null && PHRASING_ONLY.contains(top().name)
            && !(top().name.equals("code") && name.equals("pre"))) {
          if (FORMATTING.contains(top().name) && !top().reconstructed) {
            interrupted.add(top());
            popAndClose();
          } else {
            closeImplicitly();
          }
        }
        // Owned by the element left on top, not by each other: "<b><i>x<p>" keeps both. Added
        // outermost first - they were popped innermost first.
        for (int k = interrupted.size() - 1; k >= 0; k--) {
          Frame frame = interrupted.get(k);
          suspended.add(new Suspended(frame.name, frame.openTag, stack.size()));
        }
      }
    }

    /**
     * Before phrasing content: finds it a container that takes it and reopens interrupted formatting.
     * Text after a table's rows means the table was left open, text before its first row is a caption
     * that gets a row and a cell of its own (a table cannot hold text and closing it there would
     * leave the rows that follow without one), text in a row lost its {@code <td>} and text in a
     * list lost its {@code <li>}.
     */
    private void startPhrasing() {
      if (top() != null && TABLE_CONTAINERS.contains(top().name)) {
        if (top().hasContent) {
          closeThroughNearest(Set.of(TABLE), Set.of());
        } else {
          push("tr", "<tr>", false);
        }
      }
      if (top() != null && LISTS.contains(top().name)) {
        push("li", "<li>", false);
      } else if (top() != null && top().name.equals("tr")) {
        push("td", "<td>", false);
      }
      if (top() != null && NO_PHRASING.contains(top().name)) {
        return;
      }
      // Kept outermost first (see closeImpliedBy), so the reopened tags nest the way they were written.
      for (Suspended entry : suspended) {
        if (entry.reopened == null || !stack.contains(entry.reopened)) {
          entry.reopened = push(entry.name, entry.openTag, true);
        }
      }
    }

    private void text(String text) {
      if (!text.isBlank()) {
        startPhrasing();
        markContent();
      }
      out.add(text);
    }

    private void close(String name) {
      if (ROOT.equals(name) || VOID_ELEMENTS.contains(name)) {
        // A void element's own closing tag was consumed by open(); any other one is a stray.
        return;
      }
      Frame target = nearest(Set.of(name), Set.of());
      if (target != null) {
        while (top() != target) {
          closeImplicitly();
        }
        popAndClose();
        if (target.reconstructed) {
          forgetSuspended(name);
        }
        return;
      }
      if (forgetSuspended(name)) {
        return;
      }
      for (Set<String> family : FAMILIES) {
        Frame relative = family.contains(name) ? nearest(family, Set.of()) : null;
        if (relative != null) {
          // "<ul>...</ol>", "<h2>...</h3>": a mistyped close of the same kind of element.
          close(relative.name);
          return;
        }
      }
      Integer pending = implicitlyClosed.get(name);
      if (pending != null) {
        if (pending == 1) {
          implicitlyClosed.remove(name);
        } else {
          implicitlyClosed.put(name, pending - 1);
        }
        return;
      }
      Frame top = top();
      if (top != null && !top.reconstructed && INLINE.contains(name) && INLINE.contains(top.name)) {
        // "<i>x</b>": a mistyped close of the inline element on top, not a stray.
        popAndClose();
      }
    }

    /** Removes the latest interrupted-formatting entry for {@code name}, if any. */
    private boolean forgetSuspended(String name) {
      for (int k = suspended.size() - 1; k >= 0; k--) {
        if (suspended.get(k).name.equals(name)) {
          suspended.remove(k);
          return true;
        }
      }
      return false;
    }

    private void drain() {
      // Same as MessageMLBalancer: prune what never got content, from the top, then close the rest.
      boolean pruning = true;
      while (!stack.isEmpty()) {
        Frame frame = stack.peek();
        if (pruning && !frame.hasContent) {
          stack.pop();
          out.set(frame.openSegment, "");
          continue;
        }
        pruning = false;
        popAndClose();
      }
    }

    // --- stack helpers --------------------------------------------------------------------------

    private Frame top() {
      return stack.peek();
    }

    private Frame push(String name, String openTag, boolean reconstructed) {
      Frame frame = new Frame(name, openTag, out.size(), reconstructed);
      out.add(openTag);
      stack.push(frame);
      return frame;
    }

    private void markContent() {
      markContentIf(true);
    }

    private void markContentIf(boolean condition) {
      if (condition && top() != null) {
        top().hasContent = true;
      }
    }

    /** Pops the top element and writes its closing tag - or removes it whole, for an empty list. */
    private void popAndClose() {
      Frame frame = stack.pop();
      if (frame.unwrapped) {
        markContentIf(frame.hasContent);
      } else if (LISTS.contains(frame.name) && !frame.hasContent) {
        // "<ul></ul>" does not validate: the list goes, along with the whitespace inside it. Blanked
        // in place, never removed, so the segment indices the frames below hold cannot shift.
        for (int i = frame.openSegment; i < out.size(); i++) {
          out.set(i, "");
        }
      } else {
        out.add("</" + frame.name + ">");
        markContent();
        if (frame.name.equals("li") && top() != null) {
          top().lastItem = frame;
          top().lastItemCloseSegment = out.size() - 1;
        }
      }
      suspended.removeIf(entry -> entry.ownerDepth > stack.size());
    }

    private void closeImplicitly() {
      Frame frame = top();
      if (!frame.reconstructed) {
        implicitlyClosed.merge(frame.name, 1, Integer::sum);
      }
      popAndClose();
    }

    private void closeThrough(Frame target) {
      while (stack.contains(target)) {
        closeImplicitly();
      }
    }

    private void closeAbove(Frame target) {
      if (target != null) {
        while (top() != target) {
          closeImplicitly();
        }
      }
    }

    /** Closes through the nearest element named in {@code names}, unless {@code scope} comes first. */
    private void closeThroughNearest(Set<String> names, Set<String> scope) {
      Frame target = nearest(names, scope);
      if (target != null) {
        closeThrough(target);
      }
    }

    /** The nearest open element named in {@code names}, searching down from the top until {@code scope}. */
    private Frame nearest(Set<String> names, Set<String> scope) {
      for (Frame frame : stack) {
        if (names.contains(frame.name)) {
          return frame;
        }
        if (scope.contains(frame.name)) {
          return null;
        }
      }
      return null;
    }

    // --- tag rebuilding -------------------------------------------------------------------------

    /**
     * Returns the opening tag exactly as written when nothing in it needs fixing, so a well-formed
     * fragment survives byte for byte, otherwise a rebuilt one with a canonical name and quoted,
     * escaped, de-duplicated attributes - or {@code null} when the element lacks an attribute it
     * cannot do without and has to be unwrapped.
     */
    private String rebuildOpenTag(MessageMLTagScanner.OpenTag tag, String name, Alias alias) {
      String raw = source(tag);
      int end = raw.length() - (tag.selfClosing() ? 2 : 1);
      boolean dirty = !name.equals(tag.name());
      Map<String, String> attributes = new LinkedHashMap<>();
      // Kept apart so a value the model also wrote ("checked checked=\"false\"") is the one that counts.
      Set<String> flags = new LinkedHashSet<>();
      int pos = 1 + tag.name().length();
      while (pos < end) {
        char c = raw.charAt(pos);
        if (Character.isWhitespace(c)) {
          pos++;
          continue;
        }
        int nameStart = pos;
        while (pos < end && !Character.isWhitespace(raw.charAt(pos)) && "=\"'/".indexOf(raw.charAt(pos)) < 0) {
          pos++;
        }
        String attribute = raw.substring(nameStart, pos);
        if (attribute.isEmpty()) {
          // A quote or slash where a name should be: nothing salvageable.
          pos++;
          dirty = true;
          continue;
        }
        int afterName = skipWhitespace(raw, pos, end);
        if (afterName >= end || raw.charAt(afterName) != '=') {
          // A value-less attribute. Only the form booleans survive, with the value XML requires.
          dirty = true;
          String lower = attribute.toLowerCase(Locale.ROOT);
          if (BOOLEAN_ATTRIBUTES.contains(lower)) {
            flags.add(lower);
          }
          continue;
        }
        pos = skipWhitespace(raw, afterName + 1, end);
        String value;
        if (pos < end && (raw.charAt(pos) == '"' || raw.charAt(pos) == '\'')) {
          int closingQuote = raw.indexOf(raw.charAt(pos), pos + 1);
          if (closingQuote < 0 || closingQuote >= end) {
            // The scanner and this loop do not always pair quotes alike (<a "x='y" z>): the value
            // runs to the end of the tag.
            closingQuote = end;
            dirty = true;
          }
          value = raw.substring(pos + 1, closingQuote);
          pos = closingQuote + 1;
        } else {
          int valueStart = pos;
          while (pos < end && !Character.isWhitespace(raw.charAt(pos))) {
            pos++;
          }
          value = raw.substring(valueStart, pos);
          dirty = true;
        }
        String normalized = normalizeText(value);
        if (!normalized.equals(value)) {
          dirty = true;
        }
        if (attributes.containsKey(attribute)) {
          dirty = true;
        } else {
          attributes.put(attribute, normalized);
        }
      }
      flags.forEach(flag -> attributes.putIfAbsent(flag, "true"));

      if (name.equals("emoji") && attributes.containsKey(SHORTCODE)) {
        String shortcode = attributes.get(SHORTCODE);
        Matcher colons = EMOJI_COLONS.matcher(shortcode);
        String cleaned = EMOJI_INVALID.matcher(colons.matches() ? colons.group(1) : shortcode)
            .replaceAll("_").replaceAll("(?:^_+)|(?:_+$)", "");
        if (!cleaned.equals(shortcode)) {
          attributes.put(SHORTCODE, cleaned);
          dirty = true;
        }
      }
      List<String> required = REQUIRED_ATTRIBUTES.get(name);
      if (required != null && required.stream().allMatch(attribute -> isBlank(attributes.get(attribute)))) {
        return null;
      }
      if (alias != null && alias.style() != null) {
        String existing = attributes.get("style");
        attributes.put("style", existing == null ? alias.style() : alias.style() + " " + existing);
        dirty = true;
      }
      if (!dirty) {
        return raw;
      }
      StringBuilder rebuilt = new StringBuilder("<").append(name);
      attributes.forEach((attribute, value) ->
          rebuilt.append(' ').append(attribute).append("=\"").append(value.replace("\"", "&quot;")).append('"'));
      return rebuilt.append(tag.selfClosing() ? "/>" : ">").toString();
    }

    private static boolean isBlank(String value) {
      return value == null || value.isBlank();
    }

    private static int skipWhitespace(String raw, int pos, int end) {
      int p = pos;
      while (p < end && Character.isWhitespace(raw.charAt(p))) {
        p++;
      }
      return p;
    }
  }

  /** Lower case, with the two camel-case MessageML names and the HTML aliases resolved. */
  static String canonicalName(String rawName) {
    String lower = rawName.toLowerCase(Locale.ROOT);
    Alias alias = ALIASES.get(lower);
    if (alias != null) {
      return alias.name();
    }
    return CAMEL_CASE_NAMES.getOrDefault(lower, lower);
  }

  /**
   * Makes text (or an attribute value) XML-safe: escapes a raw {@code <} and a stray {@code &},
   * keeps numeric and XML-predefined references and turns an HTML named reference into numeric ones.
   * Already-safe text comes back unchanged.
   */
  static String normalizeText(String text) {
    if (text.indexOf('<') < 0 && text.indexOf('&') < 0) {
      return text;
    }
    StringBuilder sb = new StringBuilder(text.length() + 16);
    Matcher reference = REFERENCE.matcher(text);
    int i = 0;
    while (i < text.length()) {
      char c = text.charAt(i);
      if (c == '<') {
        sb.append("&lt;");
        i++;
      } else if (c == '&') {
        reference.region(i, text.length());
        if (!reference.lookingAt()) {
          sb.append(AMPERSAND_ENTITY);
          i++;
          continue;
        }
        String name = reference.group(1);
        String replacement = name == null || HtmlEntities.XML_PREDEFINED.contains(name)
            ? reference.group()
            : HtmlEntities.toNumericReference(name);
        if (replacement == null) {
          sb.append(AMPERSAND_ENTITY);
          i++;
        } else {
          sb.append(replacement);
          i = reference.end();
        }
      } else {
        sb.append(c);
        i++;
      }
    }
    return sb.toString();
  }

  private static Set<String> union(Set<String> a, Set<String> b) {
    Set<String> all = new HashSet<>(a);
    all.addAll(b);
    return Set.copyOf(all);
  }
}
