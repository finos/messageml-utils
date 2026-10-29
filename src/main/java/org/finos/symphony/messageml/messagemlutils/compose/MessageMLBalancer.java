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
import java.util.Comparator;
import java.util.Deque;
import java.util.List;
import java.util.Set;

/**
 * Stateless auto-close on top of {@link MessageMLTagScanner}: completes a block that
 * is missing closing tags at its end, whether because the stream is still running or because the
 * model forgot them, without touching a block whose tags are genuinely mismatched.
 *
 * <ol>
 *   <li>Cut the unfinished tail the scanner reports as truncated. A trailing {@code &...} with no
 *       {@code ;} is not cut but escaped ({@code AT&T} becomes {@code AT&amp;T}): on a finished answer
 *       it is prose, on a stream it shows as typed until the rest of the reference arrives.
 *   <li>Walk the remaining tokens with a stack: an opening tag pushes, a closing tag must match the
 *       top of the stack or the whole fragment is rejected, {@code <x/>} is skipped and {@code br},
 *       {@code hr} and {@code img} written without a trailing {@code /} are rewritten self-closing.
 *   <li>Prune: while the element on top of the stack has no content yet (nothing but whitespace
 *       since its own opening tag), its opening tag is removed and it is popped without a matching
 *       close.
 *   <li>Append closing tags for whatever remains open, innermost first.
 * </ol>
 *
 * <p>Stateless and safe to share across threads.
 */
public final class MessageMLBalancer {

  private static final Set<String> VOID_ELEMENTS = Set.of("br", "hr", "img");

  private final MessageMLTagScanner scanner = new MessageMLTagScanner();

  private static final class OpenElement {
    final MessageMLTagScanner.OpenTag tag;
    boolean hasContent;

    OpenElement(MessageMLTagScanner.OpenTag tag) {
      this.tag = tag;
    }

    boolean isVoid() {
      return VOID_ELEMENTS.contains(tag.name());
    }
  }

  private record Edit(int start, int end, String replacement) {}

  /**
   * @return the completed fragment, the same {@code String} instance when it was already balanced,
   *     or {@code null} when its tags are mismatched rather than merely cut short
   */
  public String balance(String fragment) {
    if (fragment == null) {
      return null;
    }
    MessageMLTagScanner.ScanResult scan = scanner.scan(fragment);

    Deque<OpenElement> stack = new ArrayDeque<>();
    List<Edit> edits = new ArrayList<>();

    for (MessageMLTagScanner.Token token : scan.tokens()) {
      if (!consume(fragment, token, stack, edits)) {
        return null;
      }
    }

    escapeTrailingReference(fragment, scan, edits);
    List<String> closingTags = drain(stack, edits);

    boolean alreadyBalanced = !scan.truncated() && edits.isEmpty() && closingTags.isEmpty();
    if (alreadyBalanced) {
      return fragment;
    }
    return rebuild(fragment, scan.consumedLength(), edits, closingTags);
  }

  /** @return {@code false} when the token is a closing tag that matches nothing */
  private static boolean consume(
      String fragment, MessageMLTagScanner.Token token, Deque<OpenElement> stack, List<Edit> edits) {
    if (token instanceof MessageMLTagScanner.OpenTag openTag) {
      open(openTag, stack);
    } else if (token instanceof MessageMLTagScanner.CloseTag closeTag) {
      return close(closeTag, stack, edits);
    } else if (token instanceof MessageMLTagScanner.Text text) {
      markTextContent(fragment, text, stack);
    } else {
      // Comment or Cdata: opaque content, counts toward its parent same as a self-closing tag.
      markTopHasContent(stack);
    }
    return true;
  }

  private static void open(MessageMLTagScanner.OpenTag openTag, Deque<OpenElement> stack) {
    if (openTag.selfClosing()) {
      // Unconditionally real content for the parent: a self-closing tag is never later found to
      // be "empty" the way a pushed element can be.
      markTopHasContent(stack);
      return;
    }
    // A non-self-closing void element (a model-written <br>, <hr> or <img> with no trailing "/") is
    // still pushed like any other element: a real matching close tag further along the stream
    // (messageml-utils accepts <img ...></img> as well as <img .../> and the render corpus has an
    // example of exactly that) must be allowed to pop it normally. One that is still open when its
    // parent closes or the stream ends gets rewritten self-closing instead, see close() and drain().
    //
    // Deliberately NOT marked as the parent's content yet: whether this child turns out to be real
    // content is only known once it is either properly closed or, if it survives to the drain loop,
    // kept rather than pruned. Crediting the parent eagerly here would let an element whose only
    // "content" is a child that is itself later pruned as empty (an <li> opened but never given any
    // content before the stream was cut) survive pruning too, which produces an empty <ul></ul> that
    // does not validate.
    stack.push(new OpenElement(openTag));
  }

  /** @return {@code false} when the closing tag matches no open element */
  private static boolean close(
      MessageMLTagScanner.CloseTag closeTag, Deque<OpenElement> stack, List<Edit> edits) {
    // A void element left open between its parent's opening and closing tag (<p>a<br>b</p>) takes no
    // content, so the parent's closing tag ends it: rewrite it self-closing and carry on.
    while (isOpenVoidElementOtherThan(stack.peek(), closeTag.name())) {
      selfClose(stack.pop(), edits);
      markTopHasContent(stack);
    }
    OpenElement top = stack.peek();
    if (top == null || !top.tag.name().equals(closeTag.name())) {
      return false;
    }
    stack.pop();
    // The child was explicitly, properly terminated by the source: real content for its parent
    // regardless of what the child itself contained.
    markTopHasContent(stack);
    return true;
  }

  private static boolean isOpenVoidElementOtherThan(OpenElement element, String closingName) {
    return element != null && element.isVoid() && !element.tag.name().equals(closingName);
  }

  private static void markTextContent(
      String fragment, MessageMLTagScanner.Text text, Deque<OpenElement> stack) {
    OpenElement top = stack.peek();
    if (top != null && !top.hasContent && !isBlank(fragment, text.start(), text.end())) {
      top.hasContent = true;
    }
  }

  /**
   * Drains whatever is left open, innermost (top of stack) first. A void element is rewritten
   * self-closing wherever it sits and counts as content for its parent. Otherwise, pruning removes an
   * element that never got real content, stopping for good at the first element that did - everything
   * below that point is kept and gets its closing tag appended, regardless of its own content.
   *
   * @return the names to close, in the order to append them
   */
  private static List<String> drain(Deque<OpenElement> stack, List<Edit> edits) {
    List<String> closingTags = new ArrayList<>();
    boolean pruning = true;
    while (!stack.isEmpty()) {
      OpenElement top = stack.pop();
      if (top.isVoid()) {
        selfClose(top, edits);
        markTopHasContent(stack);
        pruning = false;
      } else if (pruning && !top.hasContent) {
        edits.add(new Edit(top.tag.start(), top.tag.end(), ""));
      } else {
        pruning = false;
        closingTags.add(top.tag.name());
      }
    }
    return closingTags;
  }

  /**
   * Escapes the {@code &} of a reference left unfinished at the very end of the fragment ({@code &},
   * {@code &am}, {@code &#12}). The scanner keeps it as text: it cannot tell a stream that is about to
   * type {@code &Tau;} from a final {@code AT&T} (cutting it lost the {@code &T}).
   */
  private static void escapeTrailingReference(
      String fragment, MessageMLTagScanner.ScanResult scan, List<Edit> edits) {
    List<MessageMLTagScanner.Token> tokens = scan.tokens();
    if (scan.truncated() || tokens.isEmpty()
        || !(tokens.get(tokens.size() - 1) instanceof MessageMLTagScanner.Text text)) {
      return;
    }
    int amp = fragment.lastIndexOf('&');
    if (amp < text.start()) {
      return;
    }
    for (int k = amp + 1; k < fragment.length(); k++) {
      char c = fragment.charAt(k);
      if (!Character.isLetterOrDigit(c) && !(c == '#' && k == amp + 1)) {
        return;
      }
    }
    edits.add(new Edit(amp, amp + 1, "&amp;"));
  }

  private static String rebuild(
      String fragment, int consumedLength, List<Edit> edits, List<String> closingTags) {
    edits.sort(Comparator.comparingInt(Edit::start));
    StringBuilder body = new StringBuilder();
    int cursor = 0;
    for (Edit edit : edits) {
      body.append(fragment, cursor, edit.start());
      body.append(edit.replacement());
      cursor = edit.end();
    }
    body.append(fragment, cursor, consumedLength);

    for (String name : closingTags) {
      body.append("</").append(name).append(">");
    }
    return body.toString();
  }

  private static void selfClose(OpenElement element, List<Edit> edits) {
    edits.add(new Edit(element.tag.end() - 1, element.tag.end() - 1, "/"));
  }

  private static void markTopHasContent(Deque<OpenElement> stack) {
    OpenElement top = stack.peek();
    if (top != null) {
      top.hasContent = true;
    }
  }

  /** Index-based so the check allocates nothing: this runs for every text token of every streaming update. */
  private static boolean isBlank(String text, int start, int end) {
    for (int i = start; i < end; i++) {
      if (!Character.isWhitespace(text.charAt(i))) {
        return false;
      }
    }
    return true;
  }
}
