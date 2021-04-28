package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class SpanTest extends ElementTest {

  @Test
  public void testSpan() throws Exception {
    String input = "<messageML>Hello <span>world</span>!</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 3, messageML.getChildren().size());

    Element span = messageML.getChildren().get(1);

    assertEquals("Element class", Span.class, span.getClass());
    assertEquals("Element tag name", "span", span.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), span.getAttributes());
    assertEquals("Element children", 1, span.getChildren().size());
    assertEquals("Child element", TextNode.class, span.getChildren().get(0).getClass());
    assertEquals("Child element text", "world", span.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello <span>world</span>!</div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello world!", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testSpanInvalidAttr() throws Exception {
    String invalidAttr = "<messageML>Hello<span title=\"label\">world</span>!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"span\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSpanInvalidEntityId() throws Exception {
    String invalidAttr = "<messageML>Hello<span class=\"label\" data-entity-id=\"id\">world</span>!</messageML>";
    String entityJson = "{\"id\":{\"key\":\"value\"}}";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"data-entity-id\" is only allowed if the element class is "
        + "\"entity\".");
    context.parseMessageML(invalidAttr, entityJson, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSpanBi() throws Exception {
    String input = "<messageML><span>Hello</span> <span>world</span>!</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    BiContext biContext = context.getBiContext();
    assertEquals(2, biContext.getItems().size());

    BiItem item = biContext.getItems().get(0);
    assertEquals("Spans", item.getName());
    assertEquals(2, item.getAttributes().get("count"));
  }
}
