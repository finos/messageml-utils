package org.finos.symphony.messageml.messagemlutils.compose;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

/**
 * The slips a model makes in hand-written markup, applied mechanically to every example of the
 * "render" doc corpus: one closing tag left out, one opening tag left out, two closing tags left out
 * and two adjacent closing tags swapped. Whatever the slip, the final render must be content - never
 * the degraded card - and must keep every word of the text. The renderer only ever returns content it
 * validated, so validity comes with it.
 */
class NativeMessageMLRepairPropertiesTest {

  private static final Pattern CLOSING_TAG = Pattern.compile("</[A-Za-z][\\w-]*>");
  /** An opening tag that is not self-closing. */
  private static final Pattern OPENING_TAG = Pattern.compile("<[A-Za-z][\\w-]*(?:\\s[^>]*)?(?<!/)>");
  private static final Pattern TAG = Pattern.compile("<[^>]*>");
  private static final Pattern WHITESPACE = Pattern.compile("\\s+");

  private final NativeMessageMLRenderer renderer = new NativeMessageMLRenderer();

  @TestFactory
  Stream<DynamicTest> anySingleClosingTagLeftOut_isRepaired() {
    return perCorpusExample(example -> {
      List<int[]> closing = matches(CLOSING_TAG, example);
      List<String> mutations = new ArrayList<>();
      for (int[] tag : closing) {
        mutations.add(remove(example, tag));
      }
      return mutations;
    });
  }

  @TestFactory
  Stream<DynamicTest> anySingleOpeningTagLeftOut_isRepaired() {
    return perCorpusExample(example -> {
      List<String> mutations = new ArrayList<>();
      for (int[] tag : matches(OPENING_TAG, example)) {
        mutations.add(remove(example, tag));
      }
      return mutations;
    });
  }

  @TestFactory
  Stream<DynamicTest> anyTwoClosingTagsLeftOut_areRepaired() {
    return perCorpusExample(example -> {
      List<int[]> closing = matches(CLOSING_TAG, example);
      List<String> mutations = new ArrayList<>();
      for (int a = 0; a < closing.size(); a++) {
        for (int b = a + 1; b < closing.size(); b++) {
          int[] first = closing.get(a);
          int[] second = closing.get(b);
          mutations.add(example.substring(0, first[0]) + example.substring(first[1], second[0])
              + example.substring(second[1]));
        }
      }
      return mutations;
    });
  }

  @TestFactory
  Stream<DynamicTest> adjacentClosingTagsSwapped_areRepaired() {
    return perCorpusExample(example -> {
      List<int[]> closing = matches(CLOSING_TAG, example);
      List<String> mutations = new ArrayList<>();
      for (int k = 0; k + 1 < closing.size(); k++) {
        int[] first = closing.get(k);
        int[] second = closing.get(k + 1);
        if (first[1] == second[0]) {
          mutations.add(example.substring(0, first[0]) + example.substring(second[0], second[1])
              + example.substring(first[0], first[1]) + example.substring(second[1]));
        }
      }
      return mutations;
    });
  }

  private Stream<DynamicTest> perCorpusExample(Function<String, List<String>> mutate) {
    return MessageMLStructureRepairerTest.corpusFiles().map(file -> dynamicTest(file.getFileName().toString(), () -> {
      List<String> failures = new ArrayList<>();
      for (String mutated : mutate.apply(read(file))) {
        NativeMessageMLRenderer.Result result = renderer.render(mutated, MessageMLRenderOptions.full());
        if (result.outcome() == NativeMessageMLRenderer.Outcome.DEGRADED) {
          failures.add("degraded: " + mutated);
        } else if (!text(result.markup()).equals(text(mutated))) {
          failures.add("lost text: " + mutated + "\n  rendered as: " + result.markup());
        }
      }
      assertTrue(failures.isEmpty(), () -> String.join("\n", failures));
    }));
  }

  /** Visible text with tags removed and whitespace collapsed. */
  private static String text(String markup) {
    return WHITESPACE.matcher(TAG.matcher(markup).replaceAll("")).replaceAll(" ").strip();
  }

  private static List<int[]> matches(Pattern pattern, String example) {
    List<int[]> ranges = new ArrayList<>();
    Matcher matcher = pattern.matcher(example);
    while (matcher.find()) {
      ranges.add(new int[] {matcher.start(), matcher.end()});
    }
    return ranges;
  }

  private static String remove(String example, int[] range) {
    return example.substring(0, range[0]) + example.substring(range[1]);
  }

  private static String read(Path file) {
    try {
      return Files.readString(file).strip();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
