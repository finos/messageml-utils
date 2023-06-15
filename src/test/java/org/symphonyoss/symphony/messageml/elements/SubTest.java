package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class SubTest extends ElementTest {

  @Test
  public void testSub() throws Exception {
    String input = "<messageML><sub>Hello world!</sub></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element sub = messageML.getChildren().get(0);

    assertEquals("Element class", Sub.class, sub.getClass());
    assertEquals("Element tag name", "sub", sub.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), sub.getAttributes());
    assertEquals("Element children", 1, sub.getChildren().size());
    assertEquals("Child element", TextNode.class, sub.getChildren().get(0).getClass());
    assertEquals("Child element text", "Hello world!", sub.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\"><sub>Hello world!</sub></div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello world!", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><b class=\"label\">Hello world!</b></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    sub = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, sub.getAttributes().size());
    assertEquals("Attribute", "label", sub.getAttribute("class"));

    String styleAttr = "<messageML><b style=\"color:green\">Hello world!</b></messageML>";
    context.parseMessageML(styleAttr, null, MessageML.MESSAGEML_VERSION);
    sub = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, sub.getAttributes().size());
    assertEquals("Attribute", "color:green", sub.getAttribute("style"));
  }


  @Test
  public void testSubInvalidAttr() throws Exception {
    String invalidAttr = "<messageML>Hello <sub title=\"label\">world!</sub></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"sub\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

}
