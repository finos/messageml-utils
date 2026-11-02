package org.finos.symphony.messageml.messagemlutils.compose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * Hand-written cases for {@link MessageMLBalancer}: truncated blocks are auto-closed.
 */
class MessageMLBalancerTest {

  private final MessageMLBalancer balancer = new MessageMLBalancer();

  @Test
  void truncatedInlineElement_isClosed() {
    assertEquals("<h1>Hello</h1><b>world</b>", balancer.balance("<h1>Hello</h1><b>world"));
  }

  @Test
  void cutInsideATag_dropsIt() {
    assertEquals("<p>Intro</p>", balancer.balance("<p>Intro</p><div style=\"border:1px"));
  }

  @Test
  void freshlyOpenedListItem_isPruned() {
    assertEquals("<ul><li>A</li></ul>", balancer.balance("<ul><li>A</li><li>"));
  }

  @Test
  void freshlyOpenedList_isPruned() {
    assertEquals("<p>Intro</p>", balancer.balance("<p>Intro</p><ul>"));
  }

  @Test
  void mismatchedTags_isNotAutoClosed() {
    assertNull(balancer.balance("<b><i>x</b>"));
  }

  @Test
  void finalAnswerMissingItsClosingTags_isClosedInnermostFirst() {
    assertEquals("<card><body>Done</body></card>", balancer.balance("<card><body>Done"));
  }

  @Test
  void closedBlockWithUnclosedInnerTags_isClosedInnermostFirst() {
    // The </messageML> boundary itself is stripped before the fragment reaches the balancer, so the
    // raw fragment the balancer sees is exactly what a truncated inline element would produce too.
    assertEquals("<p><b>world</b></p>", balancer.balance("<p><b>world"));
  }

  @Test
  void closingTagMissingInTheMiddle_isNotAutoClosed() {
    assertNull(balancer.balance("<p><b>world</p>"));
  }

  @Test
  void unterminatedEntityReference_isEscapedNotCut() {
    assertEquals("&amp;am", balancer.balance("&am"));
    assertEquals("<p>AT&amp;T</p>", balancer.balance("<p>AT&T"));
    assertEquals("<p>R&amp;D</p>", balancer.balance("<p>R&D"));
    assertEquals("<p>x &amp;#12</p>", balancer.balance("<p>x &#12"));
    assertEquals("<p>Profit &amp;</p>", balancer.balance("<p>Profit &"));
  }

  @Test
  void completeEntityReferenceAtTheEnd_isLeftAlone() {
    assertEquals("<p>a &amp;</p>", balancer.balance("<p>a &amp;"));
  }

  @Test
  void ampersandFollowedBySpaceAtTheEnd_isLeftForTheRepairer() {
    assertEquals("<p>a & </p>", balancer.balance("<p>a & "));
  }

  @Test
  void unterminatedCommentAlone_isCutBackToBeforeIt() {
    assertEquals("", balancer.balance("<!-- unterminated comment"));
  }

  @Test
  void unterminatedCdataAlone_isCutBackToBeforeIt() {
    assertEquals("", balancer.balance("<![CDATA[ unterminated"));
  }

  @Test
  void voidElementWithoutTrailingSlash_isRewrittenSelfClosing() {
    assertEquals("<br/>", balancer.balance("<br>"));
  }

  @Test
  void voidElementInsideAContainer_isRewrittenSelfClosingWhenTheParentCloses() {
    assertEquals("<p>Line 1<br/>Line 2</p>", balancer.balance("<p>Line 1<br>Line 2</p>"));
  }

  @Test
  void voidElementFollowedByASiblingElement_isRewrittenSelfClosing() {
    assertEquals("<p>a<br/><b>x</b></p>", balancer.balance("<p>a<br><b>x</b></p>"));
  }

  @Test
  void voidElementWithItsOwnClosingTag_isLeftAlone() {
    String fragment = "<p><img src=\"a.png\"></img></p>";

    assertSame(fragment, balancer.balance(fragment));
  }

  @Test
  void closingTagThatMatchesNothingAfterAVoidElement_isStillMismatched() {
    assertNull(balancer.balance("<p>a<br></b>"));
  }

  @Test
  void voidElementAtTheEndOfAnOtherwiseEmptyList_keepsItsParents() {
    assertEquals("<ul><li><br/></li></ul>", balancer.balance("<ul><li><br>"));
  }

  @Test
  void whitespaceOnlyTextInsideAnOpenElement_doesNotCountAsContent() {
    assertEquals("<p>a</p>  \n ", balancer.balance("<p>a</p><ul><li>  \n "));
  }

  @Test
  void alreadyBalancedFragment_isReturnedAsTheSameInstance() {
    String fragment = "<h1>Hello</h1><b>world</b>";

    assertSame(fragment, balancer.balance(fragment));
  }

  @Test
  void nullFragment_returnsNull() {
    assertNull(balancer.balance(null));
  }
}
