package org.finos.symphony.messageml.messagemlutils.compose.format;

import org.finos.symphony.messageml.messagemlutils.compose.MessageMLRenderOptions;
import org.finos.symphony.messageml.messagemlutils.compose.NativeMessageMLRenderer;
import org.finos.symphony.messageml.messagemlutils.compose.parser.MessageMLBlock;
import org.finos.symphony.messageml.messagemlutils.compose.parser.SymphonyMarkdownExtension;

import org.commonmark.node.AbstractVisitor;
import org.commonmark.node.CustomBlock;
import org.commonmark.parser.Parser;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

/**
 * Every {@code <messageML>} example in the guide resource must actually render, not just read well -
 * a stale example would otherwise teach the model a form it would then watch fail.
 */
class SymphonyContentFormatGuideTest {

  private static final NativeMessageMLRenderer RENDERER = new NativeMessageMLRenderer();
  private static final Parser PARSER = Parser.builder()
      .extensions(List.of(SymphonyMarkdownExtension.create()))
      .build();

  @TestFactory
  Stream<DynamicTest> everyExampleBlock_rendersWithoutDegrading() {
    List<MessageMLBlock> blocks = new ArrayList<>();
    PARSER.parse(SymphonyContentFormatGuide.text()).accept(new AbstractVisitor() {
      @Override
      public void visit(CustomBlock customBlock) {
        if (customBlock instanceof MessageMLBlock block) {
          blocks.add(block);
        } else {
          super.visit(customBlock);
        }
      }
    });

    assertFalse(blocks.isEmpty(), "the guide should contain at least one <messageML> example");

    int[] index = {0};
    return blocks.stream().map(block -> {
      int n = ++index[0];
      return dynamicTest("example " + n + ": " + firstLine(block.body()), () -> {
        assertTrue(block.closed(), "example " + n + " is missing its closing </messageML>");
        NativeMessageMLRenderer.Result result = RENDERER.render(block.body(), MessageMLRenderOptions.full());
        assertNotEquals(NativeMessageMLRenderer.Outcome.DEGRADED, result.outcome(),
            () -> "example " + n + " degraded: " + block.body());
      });
    });
  }

  @Test
  void load_missingResource_throwsIllegalState() {
    assertThrows(IllegalStateException.class,
        () -> SymphonyContentFormatGuide.load("/does-not-exist.md"));
  }

  @Test
  void readAll_streamFailsMidRead_wrapsAsUncheckedIOException() {
    InputStream failing = new InputStream() {
      @Override
      public int read() throws IOException {
        throw new IOException("boom");
      }
    };

    assertThrows(IOException.class, () -> SymphonyContentFormatGuide.readAll(failing));
  }

  private static String firstLine(String body) {
    int newline = body.indexOf('\n');
    String first = newline >= 0 ? body.substring(0, newline) : body;
    return first.length() > 40 ? first.substring(0, 40) + "..." : first;
  }
}
