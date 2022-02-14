package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.TestDataProvider;

@RunWith(value = Parameterized.class)
public class CodeLanguageTest {

  @Parameterized.Parameter
  public String language;

  private MessageMLContext context;

  @Before
  public void setUp() {
    this.context = new MessageMLContext(new TestDataProvider());
  }

  @Parameterized.Parameters(name = "{index}: language - {0}")
  public static Object[] languages() {
    return new Object[] {
        "plaintext", "c", "cpp", "csharp", "css", "html", "java", "js", "jsx", "php", "python", "r", "typescript", "tsx"
    };
  }

  @Test
  public void testCodeWithLanguageAttribute() throws Exception {

    final String input = "<messageML><code language=\"" + this.language + "\">Some Code</code></messageML>";
    final String expectedPml = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code data-language=\"" + this.language + "\">Some Code</code></div>";

    this.context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals(expectedPml, this.context.getPresentationML());
  }

  @Test
  public void testCodeWithLanguageAttributeInPresentationML() throws Exception {

    final String input = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code data-language=\"" + this.language + "\">Some Code</code></div>";
    final String expectedPml = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code data-language=\"" + this.language + "\">Some Code</code></div>";

    this.context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals(expectedPml, this.context.getPresentationML());
  }

  @Test
  public void testWithUnknownLanguageAttribute() {

    final String input = "<messageML><code language=\"clojure\">Some Code</code></messageML>";

    InvalidInputException ex = assertThrows(InvalidInputException.class,
        () -> this.context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION));

    assertEquals("Attribute \"language\" of element \"code\" can only be one of the following values: [plaintext, c, cpp, csharp, css, html, java, js, jsx, php, python, r, typescript, tsx].", ex.getMessage());
  }

  @Test
  public void testWithUnknownLanguageAttributeInPresentationML() {

    final String input = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code language=\"clojure\">Some Code</code></div>";

    InvalidInputException ex = assertThrows(InvalidInputException.class,
        () -> this.context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION));

    assertEquals("Attribute \"language\" of element \"code\" can only be one of the following values: [plaintext, c, cpp, csharp, css, html, java, js, jsx, php, python, r, typescript, tsx].", ex.getMessage());
  }

  @Test
  public void testParseLanguageAttributeInMarkdown() throws Exception {
    String input = "```" + this.language + "\nSome Code\n```";
    String expectedPml = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code data-language=\"" + this.language + "\">Some Code</code></div>";

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
}
