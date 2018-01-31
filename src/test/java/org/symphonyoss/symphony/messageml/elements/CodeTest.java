package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Before;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.TestDataProvider;

public class CodeTest {

  private final IDataProvider dataProvider = new TestDataProvider();
  private MessageMLContext context;

  @Before
  public void setUp() {
    context = new MessageMLContext(dataProvider);
  }

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
    String expectedMarkdown = "```\nSystem.out.println(\"Hello`world!\");\n```\n";
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
}
