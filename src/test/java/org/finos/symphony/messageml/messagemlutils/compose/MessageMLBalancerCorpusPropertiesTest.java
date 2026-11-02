package org.finos.symphony.messageml.messagemlutils.compose;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

/**
 * The C1-C4 corpus properties, scoped to the balancer and the sanitizer in isolation, not the full
 * sanitize-balance-validate-repair pipeline. C3 at the full pipeline level is restated in
 * {@code MarkdownToMessageMLConverterNativeBlocksTest}.
 */
class MessageMLBalancerCorpusPropertiesTest {

  private static final MessageMLValidator VALIDATOR = new MessageMLValidator();
  private static final MessageMLBalancer BALANCER = new MessageMLBalancer();
  private static final MessageMLSanitizer SANITIZER = new MessageMLSanitizer();

  private static final Pattern CLOSE_TAG = Pattern.compile("</[A-Za-z][\\w:-]*>");

  /**
   * One file in {@code degrade/} whose only documented example on its source page carries no entity,
   * no form element and no FreeMarker construct (see {@code SOURCES.md}: "plain
   * {@code <mention email=.../>}, no entity"). It was still filed under {@code degrade/}
   * because the whole source page is about entities, grouped at the page level rather than
   * per-example. The sanitizer rejects on exactly three structural grounds
   * (FreeMarker, Symphony Elements, entity markers), none of which this fragment
   * trips - and it must not invent a fourth ad hoc rule just to reject a bare {@code <mention>}, which
   * is legitimately allowed elsewhere (it is in {@code render/shorthand-tags-01.mml} too). Asserted
   * explicitly below instead of silently skipped.
   */
  private static final String ENTITY_FREE_DEGRADE_FILE = "structured-objects-01.mml";

  // ---------------------------------------------------------------------------------------------
  // C1: the example validates as is and balancing it is a no-op (same String instance).
  // ---------------------------------------------------------------------------------------------

  @TestFactory
  Stream<DynamicTest> c1_renderCorpusValidatesAndIsAlreadyBalanced() {
    return corpusFiles("render").map(file -> dynamicTest(file.getFileName().toString(), () -> {
      String fragment = readFragment(file);
      String markup = "<messageML>" + fragment + "</messageML>";
      assertNull(VALIDATOR.validate(markup, null),
          () -> file.getFileName() + " was expected to validate as is");
      assertSame(fragment, BALANCER.balance(fragment),
          () -> file.getFileName() + " was expected to come back unchanged");
    }));
  }

  // ---------------------------------------------------------------------------------------------
  // C2: stripping the trailing run of closing tags and re-balancing reconstructs the example.
  //
  // "Last k closing tags" is scoped here to the maximal trailing run of </tag> occurrences that are
  // immediately adjacent to one another - zero characters between them and zero characters between
  // the last one and the end of the example - not every closing tag anywhere in the document and not
  // a run that merely has whitespace between its members. Two reasons:
  //
  // 1. Cutting at an arbitrary interior closing tag while leaving later siblings/open tags in place
  //    can change the tree the balancer would reconstruct (a later sibling can become a child of an
  //    earlier, no-longer-closed element), which is a different, more drastic property than the
  //    <b>world</messageML> case that motivates C2.
  // 2. The balancer closes a run of still-open elements back-to-back, with no separating whitespace
  //    of its own (step 4 just appends </tag> for each). Pretty-printed doc examples often have a
  //    newline and indentation between two closing tags (such as "</tfoot>\n    </table>"); cutting
  //    before the earlier of those two and asking the balancer to reconstruct both would require it
  //    to also reproduce that formatting whitespace, which is not something it is meant to do.
  // 3. The balancer always rewrites a dangling br/hr/img self-closing rather than pairing it
  //    with an appended closing tag, even when the source actually closed it with one (the render
  //    corpus's own images-and-media-02/03.mml write <img ...></img>, not self-closed). Removing such
  //    a closing tag would make the balancer legitimately reconstruct a different, equally valid
  //    document (<img .../> instead of <img ...></img>), not the original - so a void element's own
  //    closing tag is never eligible to be part of the trailing run either.
  //
  // The trailing-run reading keeps every cut a genuine prefix truncation the balancer can reconstruct
  // byte-for-byte, which is what it is actually specified to repair.
  // ---------------------------------------------------------------------------------------------

  private static final Set<String> VOID_ELEMENTS = Set.of("br", "hr", "img");

  @TestFactory
  Stream<DynamicTest> c2_strippingTrailingClosersAndRebalancing_reconstructsTheExample() {
    return corpusFiles("render").flatMap(file -> {
      String fragment = readFragment(file);
      List<int[]> trailingClosers = trailingClosingTags(fragment);
      List<DynamicTest> tests = new ArrayList<>();
      for (int k = 1; k <= trailingClosers.size(); k++) {
        int cutPoint = trailingClosers.get(trailingClosers.size() - k)[0];
        String truncated = fragment.substring(0, cutPoint);
        int kCopy = k;
        tests.add(dynamicTest(file.getFileName() + " k=" + kCopy, () ->
            assertEquals(fragment, BALANCER.balance(truncated),
                () -> "removing the last " + kCopy + " trailing closing tag(s) of "
                    + file.getFileName() + " did not reconstruct the original")));
      }
      return tests.stream();
    });
  }

  // ---------------------------------------------------------------------------------------------
  // C3: every prefix cut at a tag boundary or every 16th character never mismatches and validates
  // whenever it balances to something non-blank.
  // ---------------------------------------------------------------------------------------------

  private static final AtomicInteger TOTAL_PREFIXES = new AtomicInteger();
  private static final AtomicInteger NON_BLANK_PREFIXES = new AtomicInteger();

  @TestFactory
  Stream<DynamicTest> c3_everyPrefixOfARenderExample_neverMismatchesAndValidatesWhenNonBlank() {
    return corpusFiles("render").map(file -> dynamicTest(file.getFileName().toString(), () -> {
      String fragment = readFragment(file);
      for (int cut : prefixCutPoints(fragment)) {
        String prefix = fragment.substring(0, cut);
        String balanced = BALANCER.balance(prefix);
        assertNotNull(balanced, () -> file.getFileName() + ": prefix of length " + cut
            + " was reported as a mismatch, but every prefix of a documented example must be "
            + "auto-closeable");
        TOTAL_PREFIXES.incrementAndGet();
        if (balanced.isBlank()) {
          continue;
        }
        NON_BLANK_PREFIXES.incrementAndGet();
        String markup = "<messageML>" + balanced + "</messageML>";
        assertNull(VALIDATOR.validate(markup, null), () -> file.getFileName() + ": prefix of length "
            + cut + " balanced to non-blank content that does not validate: " + balanced);
      }
    }));
  }

  /** Cut points: right after every {@code <} and every {@code >}, plus every 16th character. */
  private static List<Integer> prefixCutPoints(String fragment) {
    TreeSet<Integer> cuts = new TreeSet<>();
    for (int i = 0; i < fragment.length(); i++) {
      char c = fragment.charAt(i);
      if (c == '<') {
        cuts.add(i);
      } else if (c == '>') {
        cuts.add(i + 1);
      }
    }
    for (int i = 16; i < fragment.length(); i += 16) {
      cuts.add(i);
    }
    cuts.remove(0);
    cuts.add(fragment.length());
    return new ArrayList<>(cuts);
  }

  /**
   * C3's content-versus-placeholder share, across the whole render corpus, measured on the first
   * green run of this test class at 580/616 prefixes (94.2%) balancing to non-blank content. Pinned
   * here at 90%, a few points below that measurement to absorb the noise a future corpus addition
   * would introduce without masking an actual regression, so a balancer change that brings back
   * flicker - more prefixes resolving to the streaming placeholder than before - fails the build
   * instead of only showing up as a quieter UX regression.
   */
  private static final int MIN_NON_BLANK_PERCENT = 90;

  @AfterAll
  static void reportAndPinContentShare() {
    int total = TOTAL_PREFIXES.get();
    if (total == 0) {
      return;
    }
    int nonBlank = NON_BLANK_PREFIXES.get();
    double percent = 100.0 * nonBlank / total;
    System.out.printf(
        "C3 content share: %d/%d prefixes (%.1f%%) balanced to non-blank content across the render "
            + "corpus%n",
        nonBlank, total, percent);
    assertTrue(percent >= MIN_NON_BLANK_PERCENT,
        () -> "C3 content share regressed to " + percent + "%, below the pinned floor of "
            + MIN_NON_BLANK_PERCENT + "% - a balancer change is showing the placeholder more often");
  }

  // ---------------------------------------------------------------------------------------------
  // C4: the sanitizer rejects every degrade/ example.
  // ---------------------------------------------------------------------------------------------

  @TestFactory
  Stream<DynamicTest> c4_degradeCorpusIsRejectedBySanitizer() {
    return corpusFiles("degrade")
        .filter(file -> !file.getFileName().toString().equals(ENTITY_FREE_DEGRADE_FILE))
        .map(file -> dynamicTest(file.getFileName().toString(), () -> {
          String fragment = readFragment(file);
          assertNull(SANITIZER.sanitize(fragment),
              () -> file.getFileName() + " was expected to be rejected by the sanitizer");
        }));
  }

  @Test
  void c4_theOneDegradeFileWithNoEntityFormOrFreemarkerContent_isNotRejected() {
    Path file = corpusFiles("degrade")
        .filter(f -> f.getFileName().toString().equals(ENTITY_FREE_DEGRADE_FILE))
        .findFirst()
        .orElseThrow(() -> new AssertionError("missing fixture " + ENTITY_FREE_DEGRADE_FILE));
    String fragment = readFragment(file);

    assertEquals(fragment, SANITIZER.sanitize(fragment));
  }

  // ---------------------------------------------------------------------------------------------
  // Shared corpus-loading helpers, mirroring parser.MessageMLDocCorpusTest's pattern - not
  // reused directly since that class is package-private in a different package.
  // ---------------------------------------------------------------------------------------------

  private static List<int[]> trailingClosingTags(String example) {
    Matcher matcher = CLOSE_TAG.matcher(example);
    List<int[]> all = new ArrayList<>();
    while (matcher.find()) {
      all.add(new int[] {matcher.start(), matcher.end()});
    }
    List<int[]> trailing = new ArrayList<>();
    int boundary = example.length();
    for (int i = all.size() - 1; i >= 0; i--) {
      int[] closer = all.get(i);
      if (closer[1] != boundary) {
        break;
      }
      String name = example.substring(closer[0] + 2, closer[1] - 1);
      if (VOID_ELEMENTS.contains(name)) {
        break;
      }
      trailing.add(0, closer);
      boundary = closer[0];
    }
    return trailing;
  }

  private static String readFragment(Path file) {
    try {
      return Files.readString(file).strip();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private static Stream<Path> corpusFiles(String bucket) {
    try {
      URL dirUrl = MessageMLBalancerCorpusPropertiesTest.class.getClassLoader()
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
