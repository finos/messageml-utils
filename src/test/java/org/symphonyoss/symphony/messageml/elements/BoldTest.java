package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class BoldTest extends ElementTest {

  @Test
  public void testBold() throws Exception {
    String input = "<messageML><b>Hello world!</b></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element bold = messageML.getChildren().get(0);

    assertEquals("Element class", Bold.class, bold.getClass());
    assertEquals("Element tag name", "b", bold.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), bold.getAttributes());
    assertEquals("Element children", 1, bold.getChildren().size());
    assertEquals("Child element", TextNode.class, bold.getChildren().get(0).getClass());
    assertEquals("Child element text", "Hello world!", bold.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\"><b>Hello world!</b></div>",
        context.getPresentationML());
    assertEquals("Markdown", "**Hello world!**", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><b class=\"label\">Hello world!</b></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    bold = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, bold.getAttributes().size());
    assertEquals("Attribute", "label", bold.getAttribute("class"));

    String styleAttr = "<messageML><b style=\"color:green\">Hello world!</b></messageML>";
    context.parseMessageML(styleAttr, null, MessageML.MESSAGEML_VERSION);
    bold = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, bold.getAttributes().size());
    assertEquals("Attribute", "color:green", bold.getAttribute("style"));
  }

  @Test
  public void testBoldInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><b title=\"label\">Hello world!</b></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"b\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }
}
