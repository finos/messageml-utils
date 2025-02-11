package org.finos.symphony.messageml.messagemlutils.markdown;

import static org.junit.jupiter.api.Assertions.*;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.util.TestDataProvider;

@Slf4j
public class MarkdownParserTest {

  @RequiredArgsConstructor
  enum Example {
    ITALIC_WITH_CODE(
        "_Some text in italic with `code formatting`_",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><i>Some text in italic with <code>code formatting</code></i></div>"
    );

    private final String markdown;
    private final String presentationML;
  }

  @ParameterizedTest
  @EnumSource(Example.class)
  void testParseMarkdown(final Example example) throws Exception {
    final MessageMLContext context = new MessageMLContext(new TestDataProvider());
    context.parseMarkdown(example.markdown, null, null);
    final String presentationML = context.getPresentationML();
    assertEquals(example.presentationML, presentationML);
  }
}
