package org.finos.symphony.messageml.messagemlutils.compose.parser;

import org.finos.symphony.messageml.messagemlutils.compose.MessageMLValidator;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

/**
 * Guards the internal consistency of the doc-derived corpus: every
 * fragment stored under {@code render/} must validate as-is against the real messageml-utils parser,
 * and every fragment under {@code upstream-invalid/} must not. This is the C1 "guards the corpus" half
 * of the corpus properties - the corpus itself is the fixture for the sanitizer and balancer tests,
 * so a stale or wrongly classified file here would silently weaken those.
 *
 * <p>{@code degrade/} is intentionally not checked here: those fragments are meant to be rejected by
 * {@code MessageMLSanitizer}, not by the raw library and some of them validate just fine on
 * their own without a sanitizer in front of them (a bare {@code <form>} with a {@code <button>} is valid
 * MessageML). Their test coverage is the sanitizer's own test suite.
 */
class MessageMLDocCorpusTest {

  private static final MessageMLValidator VALIDATOR = new MessageMLValidator();

  @TestFactory
  Stream<DynamicTest> renderCorpusValidatesAsIs() {
    return corpusFiles("render").map(file -> dynamicTest(file.getFileName().toString(), () -> {
      String fragment = readFragment(file);
      String markup = "<messageML>" + fragment + "</messageML>";
      String error = VALIDATOR.validate(markup, null);
      assertNull(error, () -> "expected " + file.getFileName() + " to validate but got: " + error);
    }));
  }

  @TestFactory
  Stream<DynamicTest> upstreamInvalidCorpusNeverValidates() {
    return corpusFiles("upstream-invalid").map(file -> dynamicTest(file.getFileName().toString(), () -> {
      String fragment = readFragment(file);
      String markup = "<messageML>" + fragment + "</messageML>";
      String error = VALIDATOR.validate(markup, null);
      assertNotNull(error, () -> file.getFileName()
          + " was expected to stay rejected by the pinned messageml-utils - promote it to render/ "
          + "if a library upgrade now accepts it");
    }));
  }

  /**
   * The file's raw content, trimmed of the trailing newline the editor added when the corpus was
   * written. Trimming matters for at least one element: {@code <chime/>} is rejected by messageml-utils
   * as soon as it has any sibling text node, including a lone trailing newline.
   */
  private static String readFragment(Path file) {
    try {
      return Files.readString(file).strip();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private static Stream<Path> corpusFiles(String bucket) {
    try {
      URL dirUrl = MessageMLDocCorpusTest.class.getClassLoader()
          .getResource("compose/messageml-doc-corpus/" + bucket);
      assertNotNull(dirUrl, () -> "missing corpus directory: compose/messageml-doc-corpus/" + bucket);
      Path dir = Paths.get(dirUrl.toURI());
      try (Stream<Path> listing = Files.list(dir)) {
        List<Path> files = listing
            .filter(p -> p.getFileName().toString().endsWith(".mml"))
            .sorted(Comparator.comparing(p -> p.getFileName().toString()))
            .toList();
        return files.stream();
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    } catch (URISyntaxException e) {
      throw new RuntimeException(e);
    }
  }
}
