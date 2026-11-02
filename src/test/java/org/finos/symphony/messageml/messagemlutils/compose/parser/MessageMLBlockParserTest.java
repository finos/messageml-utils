package org.finos.symphony.messageml.messagemlutils.compose.parser;

import org.commonmark.ext.gfm.tables.TablesExtension;
import org.commonmark.node.AbstractVisitor;
import org.commonmark.node.CustomBlock;
import org.commonmark.parser.Parser;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Block-detection tests for {@link MessageMLBlockParser}: shape of the
 * parsed AST only, independent of how {@code MessageMLBlock} is later rendered.
 */
class MessageMLBlockParserTest {

  private final Parser parser = Parser.builder()
      .extensions(List.of(TablesExtension.create(), SymphonyMarkdownExtension.create()))
      .build();

  private List<MessageMLBlock> blocksIn(String markdown) {
    List<MessageMLBlock> found = new ArrayList<>();
    parser.parse(markdown).accept(new AbstractVisitor() {
      @Override
      public void visit(CustomBlock customBlock) {
        if (customBlock instanceof MessageMLBlock block) {
          found.add(block);
        } else {
          super.visit(customBlock);
        }
      }
    });
    return found;
  }

  @Test
  void blankLineInsideABlock_keepsBothElementsInTheSameBlock() {
    String md = """
        <messageML>
        <h1>One</h1>

        <h1>Two</h1>
        </messageML>""";

    List<MessageMLBlock> blocks = blocksIn(md);

    assertEquals(1, blocks.size());
    assertEquals("<h1>One</h1>\n\n<h1>Two</h1>", blocks.get(0).body());
    assertTrue(blocks.get(0).closed());
  }

  @Test
  void twoBlocksWithMarkdownBetween_areTwoSeparateBlocks() {
    String md = """
        Intro.

        <messageML>
        <h1>First</h1>
        </messageML>

        Middle.

        <messageML>
        <h1>Second</h1>
        </messageML>

        Outro.""";

    List<MessageMLBlock> blocks = blocksIn(md);

    assertEquals(2, blocks.size());
    assertEquals("<h1>First</h1>", blocks.get(0).body());
    assertEquals("<h1>Second</h1>", blocks.get(1).body());
  }

  @Test
  void fencedExample_isIgnored() {
    String md = """
        ```
        <messageML>
        <h1>Not a block</h1>
        </messageML>
        ```""";

    assertTrue(blocksIn(md).isEmpty());
  }

  @Test
  void blockInsideAListItem_isIgnored() {
    String md = """
        - item one
          <messageML>
          <h1>Not a block</h1>
          </messageML>""";

    assertTrue(blocksIn(md).isEmpty());
  }

  @Test
  void singleLineBlock_isOneBlockWithThatLineAsBody() {
    String md = "<messageML><card><body>x</body></card></messageML>";

    List<MessageMLBlock> blocks = blocksIn(md);

    assertEquals(1, blocks.size());
    assertEquals("<card><body>x</body></card>", blocks.get(0).body());
    assertTrue(blocks.get(0).closed());
  }

  @Test
  void singleLineBlock_doesNotSwallowFollowingContent() {
    String md = """
        <messageML><card/></messageML>

        Following paragraph.""";

    List<MessageMLBlock> blocks = blocksIn(md);

    assertEquals(1, blocks.size());
    assertEquals("<card/>", blocks.get(0).body());
  }

  @Test
  void nestedOpeningTag_isTreatedAsInvalidContentOfTheOuterBlock() {
    String md = """
        <messageML>
        <messageML>
        <h1>Inner</h1>
        </messageML>""";

    List<MessageMLBlock> blocks = blocksIn(md);

    assertEquals(1, blocks.size());
    assertEquals("<messageML>\n<h1>Inner</h1>", blocks.get(0).body());
    assertTrue(blocks.get(0).closed());
  }

  @Test
  void unterminatedBlockAtEndOfInput_isNotClosed() {
    String md = """
        <messageML>
        <h1>Hello</h1>
        <b>world""";

    List<MessageMLBlock> blocks = blocksIn(md);

    assertEquals(1, blocks.size());
    assertEquals("<h1>Hello</h1>\n<b>world", blocks.get(0).body());
    assertFalse(blocks.get(0).closed());
  }

  @Test
  void openingTagInTheMiddleOfALine_isNotABlock() {
    assertTrue(blocksIn("Some text <messageML> more text").isEmpty());
  }

  @Test
  void indentedOpeningTag_isCodeNotABlock() {
    assertTrue(blocksIn("    <messageML>\n    <h1>x</h1>\n    </messageML>").isEmpty());
  }

  @Test
  void textAfterTheClosingTagOnItsLine_isKeptAsTrailingTextNotAsBody() {
    String md = """
        <messageML>
        <h1>Hello</h1>
        </messageML> Note: see above""";

    List<MessageMLBlock> blocks = blocksIn(md);

    assertEquals(1, blocks.size());
    assertEquals("<h1>Hello</h1>", blocks.get(0).body());
    assertEquals("Note: see above", blocks.get(0).trailingText());
  }

  @Test
  void textAfterTheClosingTagOfASingleLineBlock_isKeptAsTrailingText() {
    List<MessageMLBlock> blocks = blocksIn("<messageML><card/></messageML> some follow-up text");

    assertEquals(1, blocks.size());
    assertEquals("<card/>", blocks.get(0).body());
    assertEquals("some follow-up text", blocks.get(0).trailingText());
    assertTrue(blocks.get(0).closed());
  }

  @Test
  void closingTagAlone_hasNoTrailingText() {
    List<MessageMLBlock> blocks = blocksIn("<messageML>\n<card/>\n</messageML>   ");

    assertEquals("", blocks.get(0).trailingText());
  }

  @Test
  void indentedClosingTag_doesNotAddABlankBodyLine() {
    String md = """
        <messageML>
        <h1>Hello</h1>
          </messageML>""";

    List<MessageMLBlock> blocks = blocksIn(md);

    assertEquals("<h1>Hello</h1>", blocks.get(0).body());
  }

  @Test
  void whitespaceAfterTheOpeningTag_doesNotAddABlankFirstBodyLine() {
    String md = "<messageML> \n<h1>Hello</h1>\n</messageML>";

    List<MessageMLBlock> blocks = blocksIn(md);

    assertEquals("<h1>Hello</h1>", blocks.get(0).body());
  }

  @Test
  void whitespaceInsideTheTags_stillOpensAndClosesABlock() {
    String md = "<messageML >\n<h1>Hello</h1>\n</messageML  >";

    List<MessageMLBlock> blocks = blocksIn(md);

    assertEquals(1, blocks.size());
    assertEquals("<h1>Hello</h1>", blocks.get(0).body());
    assertTrue(blocks.get(0).closed());
  }

  @Test
  void openingTagWithAttributes_isNotABlock() {
    assertTrue(blocksIn("<messageML version=\"2\">\n<h1>x</h1>\n</messageML>").isEmpty());
  }
}
