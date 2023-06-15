package org.symphonyoss.symphony.messageml.elements;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

import static org.junit.Assert.assertEquals;

public class SupTest extends ElementTest {

  @Test
  public void testSup() throws Exception {
    String input = "<messageML>Hello <sup>world!</sup></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 2, messageML.getChildren().size());

    Element sup = messageML.getChildren().get(1);

    assertEquals("Element class", Sup.class, sup.getClass());
    assertEquals("Element tag name", "sup", sup.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), sup.getAttributes());
    assertEquals("Element children", 1, sup.getChildren().size());
    assertEquals("Child element", TextNode.class, sup.getChildren().get(0).getClass());
    assertEquals("Child element text", "world!", sup.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello <sup>world!</sup></div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello world!", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><b class=\"label\">Hello world!</b></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    sup = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, sup.getAttributes().size());
    assertEquals("Attribute", "label", sup.getAttribute("class"));

    String styleAttr = "<messageML><b style=\"color:green\">Hello world!</b></messageML>";
    context.parseMessageML(styleAttr, null, MessageML.MESSAGEML_VERSION);
    sup = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, sup.getAttributes().size());
    assertEquals("Attribute", "color:green", sup.getAttribute("style"));
  }


  @Test
  public void testSupInvalidAttr() throws Exception {
    String invalidAttr = "<messageML>Hello <sup title=\"label\">world!</sup></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"sup\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

}
