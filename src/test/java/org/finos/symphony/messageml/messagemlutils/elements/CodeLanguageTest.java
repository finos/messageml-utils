package org.finos.symphony.messageml.messagemlutils.elements;

import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.util.TestDataProvider;

import java.util.stream.Stream;

public class CodeLanguageTest {

  private MessageMLContext context;

  @BeforeEach
  public void setUp() {
    this.context = new MessageMLContext(new TestDataProvider());
  }

  public static Stream<Arguments> languages() {
    return Stream.of("plaintext", "c", "cpp", "csharp", "css", "html", "java", "js", "jsx", "php",
            "python", "r", "typescript", "tsx", "markdown", "json", "scala", "shell", "yaml")
        .map(Arguments::of);
  }

  @ParameterizedTest
  @MethodSource("languages")
  public void testCodeWithLanguageAttribute(String language) throws Exception {

    final String input = "<messageML><code language=\"" + language + "\">Some Code</code></messageML>";
    final String expectedPml = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code data-language=\"" + language + "\">Some Code</code></div>";

    this.context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals(expectedPml, this.context.getPresentationML());
  }

  @ParameterizedTest
  @MethodSource("languages")
  public void testCodeWithLanguageAttributeInPresentationML(String language) throws Exception {

    final String input = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code data-language=\"" + language + "\">Some Code</code></div>";
    final String expectedPml = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code data-language=\"" + language + "\">Some Code</code></div>";

    this.context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals(expectedPml, this.context.getPresentationML());
  }

  @Test
  public void testWithUnknownLanguageAttribute() {

    final String input = "<messageML><code language=\"clojure\">Some Code</code></messageML>";

    InvalidInputException ex = assertThrows(InvalidInputException.class,
        () -> this.context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION));

    assertEquals(
        "Attribute \"language\" of element \"code\" can only be one of the following values: "
            + "[plaintext, c, cpp, csharp, css, html, java, js, jsx, php, python, r, typescript, "
            + "tsx, markdown, json, scala, shell, yaml].",
        ex.getMessage());
  }

  @Test
  public void testWithUnknownLanguageAttributeInPresentationML() {

    final String input = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code language=\"clojure\">Some Code</code></div>";

    InvalidInputException ex = assertThrows(InvalidInputException.class,
        () -> this.context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION));

    assertEquals(
        "Attribute \"language\" of element \"code\" can only be one of the following values: "
            + "[plaintext, c, cpp, csharp, css, html, java, js, jsx, php, python, r, typescript, "
            + "tsx, markdown, json, scala, shell, yaml].",
        ex.getMessage());
  }

  @ParameterizedTest
  @MethodSource("languages")
  public void testParseLanguageAttributeInMarkdown(String language) throws Exception {
    String input = "```" + language + "\nSome Code\n```";
    String expectedPml = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code data-language=\"" + language + "\">Some Code</code></div>";

    this.context.parseMarkdown(input, null, null);

    assertEquals(expectedPml, this.context.getPresentationML());
  }

  @Test
  public void testUnknownAttributeIsNotAllowed() {

    final String input = "<messageML><code foo=\"bar\">Some Code</code></messageML>";

    InvalidInputException ex = assertThrows(InvalidInputException.class,
        () -> this.context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION));

    assertEquals("Attribute \"foo\" is not allowed in \"code\"", ex.getMessage());
  }

  @Test
  public void testPreformattedInsideCodeIsAllowed() throws Exception {
    final String input = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code language=\"plaintext\"><pre>Hello</pre></code></div>";

    this.context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("<div data-format=\"PresentationML\" data-version=\"2.0\"><code data-language=\"plaintext\"><pre>Hello</pre></code></div>",
        this.context.getPresentationML());
  }
}
