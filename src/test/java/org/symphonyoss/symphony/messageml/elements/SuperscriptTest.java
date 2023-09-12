package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class SuperscriptTest extends ElementTest {

  @Test
  public void testSuperscript() throws Exception {
    String input = "<messageML><sup>Hello world!</sup></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element superscript = messageML.getChildren().get(0);

    assertEquals("Element class", Superscript.class, superscript.getClass());
    assertEquals("Element tag name", "sup", superscript.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), superscript.getAttributes());
    assertEquals("Element children", 1, superscript.getChildren().size());
    assertEquals("Child element", TextNode.class, superscript.getChildren().get(0).getClass());
    assertEquals("Child element text", "Hello world!", superscript.getChildren().get(0).asText());
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><sup>Hello world!</sup></div>",
        context.getPresentationML());
    assertEquals("Markdown", "^Hello world!^", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance),
        context.getEntities());

    String withAttr = "<messageML><sup class=\"label\">Hello world!</sup></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    superscript = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, superscript.getAttributes().size());
    assertEquals("Attribute", "label", superscript.getAttribute("class"));

    String styleAttr = "<messageML><sup style=\"background:red\">Hello world!</sup></messageML>";
    context.parseMessageML(styleAttr, null, MessageML.MESSAGEML_VERSION);
    superscript = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, superscript.getAttributes().size());
    assertEquals("Attribute", "background:red", superscript.getAttribute("style"));
  }

  @Test
  public void testSuperscriptInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><sup title=\"label\">Hello world!</sup></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"sup\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSuperscriptWithinParagraph() throws Exception {
    String input = "<messageML><p>Hello <sup>World</sup>!</p></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><p>Hello "
            + "<sup>World</sup>!</p></div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello ^World^!\n\n", context.getMarkdown());
    assertEquals("Plaintext", "Hello World!", context.getText());
  }

  @Test
  public void testSuperscriptWithinBold() throws Exception {
    String input = "<messageML><b>Hello <sup>World</sup></b></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><b>Hello "
            + "<sup>World</sup></b></div>",
        context.getPresentationML());
    assertEquals("Markdown", "**Hello ^World^**", context.getMarkdown());
    assertEquals("Plaintext", "Hello World", context.getText());
  }
}
