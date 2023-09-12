package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class SubscriptTest extends ElementTest {

  @Test
  public void testSubscript() throws Exception {
    String input = "<messageML><sub>Hello world!</sub></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element subscript = messageML.getChildren().get(0);

    assertEquals("Element class", Subscript.class, subscript.getClass());
    assertEquals("Element tag name", "sub", subscript.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), subscript.getAttributes());
    assertEquals("Element children", 1, subscript.getChildren().size());
    assertEquals("Child element", TextNode.class, subscript.getChildren().get(0).getClass());
    assertEquals("Child element text", "Hello world!", subscript.getChildren().get(0).asText());
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><sub>Hello world!</sub></div>",
        context.getPresentationML());
    assertEquals("Markdown", "~Hello world!~", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance),
        context.getEntities());

    String withAttr = "<messageML><sub class=\"label\">Hello world!</sub></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    subscript = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, subscript.getAttributes().size());
    assertEquals("Attribute", "label", subscript.getAttribute("class"));

    String styleAttr = "<messageML><sub style=\"background:red\">Hello world!</sub></messageML>";
    context.parseMessageML(styleAttr, null, MessageML.MESSAGEML_VERSION);
    subscript = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, subscript.getAttributes().size());
    assertEquals("Attribute", "background:red", subscript.getAttribute("style"));
  }

  @Test
  public void testSubscriptInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><sub title=\"label\">Hello world!</sub></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"sub\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSubscriptWithinParagraph() throws Exception {
    String input = "<messageML><p>Hello <sub>World</sub>!</p></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><p>Hello "
            + "<sub>World</sub>!</p></div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello ~World~!\n\n", context.getMarkdown());
    assertEquals("Plaintext", "Hello World!", context.getText());
  }

  @Test
  public void testSubscriptWithinBold() throws Exception {
    String input = "<messageML><b>Hello <sub>World</sub></b></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><b>Hello "
            + "<sub>World</sub></b></div>",
        context.getPresentationML());
    assertEquals("Markdown", "**Hello ~World~**", context.getMarkdown());
    assertEquals("Plaintext", "Hello World", context.getText());
  }
}
