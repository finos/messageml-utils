package org.finos.symphony.messageml.messagemlutils.compose.format;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class OutputFormatContractTest {

  @Test
  void block_startsWithTheSectionHeader() {
    assertTrue(OutputFormatContract.block().startsWith("[OUTPUT FORMAT]"));
  }

  @Test
  void block_teachesTheEntityAndContainerForms() {
    String block = OutputFormatContract.block();

    assertTrue(block.contains("@[Jane Doe](mailto:jane.doe@example.com)"));
    assertTrue(block.contains("@[Jane Doe](sym:user/12345)"));
    assertTrue(block.contains("#[quarterly-review]"));
    assertTrue(block.contains("$[AAPL]"));
    assertTrue(block.contains(":::card"));
    assertTrue(block.contains(":::expand"));
  }

  /** The model copies tool-provided Symphony links and otherwise writes the plain form. */
  @Test
  void block_teachesTheSymphonyLinkRule() {
    String block = OutputFormatContract.block();

    assertTrue(block.contains("when a tool result gives a link, copy it exactly as"), block);
    assertTrue(block.contains("<https://open.symphony.com/?streamId={streamId}&streamType=chatroom>"), block);
    assertTrue(block.contains("<https://open.symphony.com/?streamId={streamId}&streamType=im>"), block);
    assertTrue(block.contains("<https://open.symphony.com/?streamId={streamId}&messageId={messageId}>"), block);
    assertTrue(block.contains("Never invent an ID."), block);
    assertTrue(block.contains("Without the IDs, name the room or message in plain text."), block);
  }

  /**
   * The block must survive expression-template engines that evaluate any {@code #{...}} they find. A
   * stray one would be interpreted as an expression, not shown to the model as literal text.
   */
  @Test
  void block_containsNoExpressionDelimiter() {
    assertFalse(OutputFormatContract.block().contains("#{"), OutputFormatContract.block());
  }

  /** The block must also survive {@code String.format}, which interprets a stray {@code %}. */
  @Test
  void block_containsNoStrayPercent() {
    assertFalse(OutputFormatContract.block().contains("%"));
  }

  @Test
  void block_neverNamesTheOutputDialect() {
    String block = OutputFormatContract.block().toLowerCase();
    assertFalse(block.contains("messageml"));
    assertFalse(block.contains("html"));
    assertFalse(block.contains("xml"));
  }

  @Test
  void blockWithFlagFalse_isByteIdenticalToBlock() {
    assertTrue(OutputFormatContract.block().equals(OutputFormatContract.block(false)));
  }

  @Test
  void blockWithFlagFalse_neverNamesTheOutputDialect() {
    String block = OutputFormatContract.block(false).toLowerCase();
    assertFalse(block.contains("messageml"));
    assertFalse(block.contains("html"));
    assertFalse(block.contains("xml"));
  }

  @Test
  void blockWithFlagTrue_appendsTheNativeBlockSectionAfterTheBaseBlock() {
    String base = OutputFormatContract.block();
    String withNative = OutputFormatContract.block(true);

    assertTrue(withNative.startsWith(base));
    assertTrue(withNative.length() > base.length());
  }

  @Test
  void blockWithFlagTrue_pointsToTheContentFormatGuide() {
    assertTrue(OutputFormatContract.block(true).contains("Read the content format guide once before"));
  }

  @Test
  void blockWithFlagTrue_containsNoExpressionDelimiter() {
    String block = OutputFormatContract.block(true);
    assertFalse(block.contains("#{"), block);
  }

  @Test
  void blockWithFlagTrue_containsNoStrayPercent() {
    assertFalse(OutputFormatContract.block(true).contains("%"));
  }
}
