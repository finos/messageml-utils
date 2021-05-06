package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class CodeTest extends ElementTest {

  @Test
  public void testCode() throws Exception {
    String input = "<messageML><code>System.out.println(\"Hello world!\");</code></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>System.out.println(&quot;Hello world!&quot;);</code></div>";
    String expectedMarkdown = "```\nSystem.out.println(\"Hello world!\");\n```\n";
    String expectedText = "System.out.println(\"Hello world!\");";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeWithEmbeddedMarkdown() throws Exception {
    String input = "<messageML><code>_Hello_ **world!**</code></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>_Hello_ **world!**</code></div>";
    String expectedMarkdown = "```\n\\_Hello\\_ \\*\\*world!\\*\\*\n```\n";
    String expectedText = "_Hello_ **world!**";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeWithMessageMLTags() throws Exception {
    String input = "<messageML><code><i>Hello</i> <b>world!</b></code></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code><i>Hello</i> <b>world!</b></code></div>";
    String expectedMarkdown = "```\nHello world!\n```\n";
    String expectedText = "Hello world!";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeWithInvalidMessageMLTag() throws Exception {
    String input = "<messageML><code><html><i>Hello</i> <b>world!</b></html></code></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Invalid MessageML content at element \"html\"", e.getMessage());
    }
  }

  @Test
  public void testCodeWithEscapedTags() throws Exception {
    String input = "<messageML><code>&lt;i&gt;Hello&lt;/i&gt; &lt;b&gt;world!&lt;/b&gt;</code></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>&lt;i&gt;Hello&lt;/i&gt; &lt;b&gt;world!&lt;/b&gt;</code></div>";
    String expectedMarkdown = "```\n<i>Hello</i> <b>world!</b>\n```\n";
    String expectedText = "<i>Hello</i> <b>world!</b>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeWithEscapedInvalidMesasgeMLTags() throws Exception {
    String input = "<messageML><code>&lt;html&gt;<i>Hello</i> <b>world!</b>&lt;/html&gt;</code></messageML>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>&lt;html&gt;<i>Hello</i> <b>world!</b>&lt;/html&gt;</code></div>";
    String expectedMarkdown = "```\n<html>Hello world!</html>\n```\n";
    String expectedText = "<html>Hello world!</html>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeInvalidParent() throws Exception {
    String input = "<messageML><b><code>System.out.println(\"Hello world!\");</code></b></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Element \"code\" is not allowed in \"b\"", e.getMessage());
    }
  }

  @Test
  public void testCodeInvalidChild() throws Exception {
    String input = "<messageML><code><table><tbody><tr><td>Invalid</td></tr></tbody></table></code></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Element \"table\" is not allowed in \"code\"", e.getMessage());
    }
  }

  @Test
  public void testCodeByPresentationML() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>System.out.println(\"Hello world!\");</code></div>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>System.out.println(&quot;Hello world!&quot;);</code></div>";
    String expectedMarkdown = "```\nSystem.out.println(\"Hello world!\");\n```\n";
    String expectedText = "System.out.println(\"Hello world!\");";

    context.parseMessageML(input, null, null);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeWithEmbeddedMarkdownByPresentationML() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code>_Hello_ **world!**</code></div>";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>_Hello_ **world!**</code></div>";
    String expectedMarkdown = "```\n\\_Hello\\_ \\*\\*world!\\*\\*\n```\n";
    String expectedText = "_Hello_ **world!**";

    context.parseMessageML(input, null, null);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeByMarkdown() throws Exception {
    String input = "\n```\nSystem.out.println(\"Hello world!\");\n```\n";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>System.out.println(&quot;Hello world!&quot;);</code></div>";
    String expectedMarkdown = "```\nSystem.out.println(\"Hello world!\");\n```\n";
    String expectedText = "System.out.println(\"Hello world!\");";

    context.parseMarkdown(input, null, null);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeByMarkdownNoNewlineDelimiters() throws Exception {
    String input = "```System.out.println(\"Hello world!\");```";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>System.out.println(&quot;Hello world!&quot;);</code></div>";
    String expectedMarkdown = "```\nSystem.out.println(\"Hello world!\");\n```\n";
    String expectedText = "System.out.println(\"Hello world!\");";

    context.parseMarkdown(input, null, null);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeByMarkdownSingleBacktick() throws Exception {
    String input = "`System.out.println(\"Hello world!\");`";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>System.out.println(&quot;Hello world!&quot;);</code></div>";
    String expectedMarkdown = "```\nSystem.out.println(\"Hello world!\");\n```\n";
    String expectedText = "System.out.println(\"Hello world!\");";

    context.parseMarkdown(input, null, null);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeByMarkdownDoubleBacktick() throws Exception {
    String input = "``System.out.println(\"Hello`world!\");``";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>System.out.println(&quot;Hello`world!&quot;);</code></div>";
    String expectedMarkdown = "```\nSystem.out.println(\"Hello\\`world!\");\n```\n";
    String expectedText = "System.out.println(\"Hello`world!\");";

    context.parseMarkdown(input, null, null);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeByMarkdownTildeDelimitedBlock() throws Exception {
    String input = "\n~~~\nSystem.out.println(\"Hello world!\");\n~~~\n";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "~~~System.out.println(&quot;Hello world!&quot;);~~~</div>";
    String expectedMarkdown = "~~~System.out.println(\"Hello world!\");~~~";
    String expectedText = "~~~System.out.println(\"Hello world!\");~~~";

    context.parseMarkdown(input, null, null);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeWithBreaks() throws Exception {
    String inputMarkdown = "```\nfoo\nbar\n```";
    context.parseMarkdown(inputMarkdown, null, null);
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><code>foo\nbar</code></div>",
        context.getPresentationML());
    assertEquals("Plaintext", "foo\nbar", context.getText());

    inputMarkdown = "```\nfoo\n\nbar\n```"; //add more new lines
    context.parseMarkdown(inputMarkdown, null, null);
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><code>foo\n\nbar</code></div>",
        context.getPresentationML());
    assertEquals("Plaintext", "foo\n\nbar", context.getText());

    inputMarkdown = "\n```\nfoo\n\nbar\n```\n"; //add leading/trailing newlines
    context.parseMarkdown(inputMarkdown, null, null);
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><code>foo\n\nbar</code></div>",
        context.getPresentationML());
    assertEquals("Plaintext", "foo\n\nbar", context.getText());

    String inputPresentationMl = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code>foo\nbar</code></div>";
    context.parseMessageML(inputPresentationMl, null, MessageML.MESSAGEML_VERSION);
    assertEquals("Markdown", "```\nfoo bar\n```\n", context.getMarkdown());
    assertEquals("Plaintext", "foo\nbar", context.getText());

    inputPresentationMl = "<div data-format=\"PresentationML\" data-version=\"2.0\"><code>\nfoo\n\nbar\n</code></div>";
    context.parseMessageML(inputPresentationMl, null, MessageML.MESSAGEML_VERSION);
    assertEquals("Markdown", "```\n foo bar \n```\n", context.getMarkdown());
    assertEquals("Plaintext", "\nfoo\n\nbar\n", context.getText());

    inputPresentationMl =
        "<div data-format=\"PresentationML\" data-version=\"2.0\">\n<code>\nfoo\n\nbar\n</code>\n</div>";
    context.parseMessageML(inputPresentationMl, null, MessageML.MESSAGEML_VERSION);
    assertEquals("Markdown", " \n```\n foo bar \n```\n ", context.getMarkdown());
    assertEquals("Plaintext", " \nfoo\n\nbar\n ", context.getText());

    String inputMesageML = "<messageML><code>foo\nbar</code></messageML>";
    context.parseMessageML(inputMesageML, null, MessageML.MESSAGEML_VERSION);
    assertEquals("Markdown", "```\nfoo bar\n```\n", context.getMarkdown());
    assertEquals("Plaintext", "foo\nbar", context.getText());

    inputMesageML = "<messageML><code>\nfoo\n\nbar\n</code></messageML>";
    context.parseMessageML(inputMesageML, null, MessageML.MESSAGEML_VERSION);
    assertEquals("Markdown", "```\n foo bar \n```\n", context.getMarkdown());
    assertEquals("Plaintext", "\nfoo\n\nbar\n", context.getText());

    inputMesageML = "<messageML>\n<code>\nfoo\n\nbar\n</code>\n</messageML>";
    context.parseMessageML(inputMesageML, null, MessageML.MESSAGEML_VERSION);
    assertEquals("Markdown", " \n```\n foo bar \n```\n ", context.getMarkdown());
    assertEquals("Plaintext", " \nfoo\n\nbar\n ", context.getText());
  }

  @Test
  public void testCodeWithMessageMLTagsByMarkdown() throws Exception {
    String input = "\n```\n<html><i>Hello</i> <b>world!</b></html>\n```\n";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>&lt;html&gt;&lt;i&gt;Hello&lt;/i&gt; &lt;b&gt;world!&lt;/b&gt;&lt;/html&gt;</code></div>";
    String expectedMarkdown = "```\n<html><i>Hello</i> <b>world!</b></html>\n```\n";
    String expectedText = "<html><i>Hello</i> <b>world!</b></html>";

    context.parseMarkdown(input, null, null);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testParseMarkdownCode() throws Exception {
    String message = "This is code:\n```\nval message: String = \"Hello world\"\nprintln message\n```\nThis is text";
    context.parseMarkdown(message, null, null);
    String presentationML = context.getPresentationML();
    System.out.println(presentationML);
  }

  @Test
  public void testCodeWithEmbeddedMarkdownByMarkdown() throws Exception {
    String input = "\n```\n_Hello_ **world!**\n```\n";
    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<code>_Hello_ **world!**</code></div>";
    String expectedMarkdown = "```\n\\_Hello\\_ \\*\\*world!\\*\\*\n```\n";
    String expectedText = "_Hello_ **world!**";

    context.parseMarkdown(input, null, null);

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Text", expectedText, context.getText());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCodeBi() throws Exception {
    String input = "<messageML>" +
        "<code>System.out.println(\"Hello world!\");</code>" +
        "<code>_Hello_ **world!**</code></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    BiContext biContext = context.getBiContext();
    assertEquals(2, biContext.getItems().size());

    BiItem item = biContext.getItems().get(0);
    assertEquals(BiFields.CODE.getValue(), item.getName());
    assertEquals(2, item.getAttributes().get(BiFields.COUNT.getValue()));
  }
}
