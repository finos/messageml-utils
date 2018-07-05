package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class DivTest extends ElementTest {

  @Test
  public void testDiv() throws Exception {
    String input = "<messageML>Hello <div>world</div>!</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 3, messageML.getChildren().size());

    Element div = messageML.getChildren().get(1);

    assertEquals("Element class", Div.class, div.getClass());
    assertEquals("Element tag name", "div", div.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), div.getAttributes());
    assertEquals("Element children", 1, div.getChildren().size());
    assertEquals("Child element", TextNode.class, div.getChildren().get(0).getClass());
    assertEquals("Child element text", "world", div.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello <div>world</div>!</div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello \n\nworld\n\n!", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML>Hello<div class=\"entity\" data-entity-id=\"id\">world</div>!</messageML>";
    String entityJson = "{\"id\":{\"key\":\"value\"}}";
    context.parseMessageML(withAttr, entityJson, MessageML.MESSAGEML_VERSION);
    div = context.getMessageML().getChildren().get(1);
    assertEquals("Attribute count", 2, div.getAttributes().size());
    assertEquals("Attribute", "entity", div.getAttribute("class"));
    assertEquals("Attribute", "id", div.getAttribute("data-entity-id"));
  }

  @Test
  public void testDivInvalidAttr() throws Exception {
    String invalidAttr = "<messageML>Hello<div title=\"label\">world</div>!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"div\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDivInvalidEntityId() throws Exception {
    String invalidAttr = "<messageML>Hello<div class=\"label\" data-entity-id=\"id\">world</div>!</messageML>";
    String entityJson = "{\"id\":{\"key\":\"value\"}}";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"data-entity-id\" is only allowed if the element class is "
        + "\"entity\".");
    context.parseMessageML(invalidAttr, entityJson, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDivInvaliIconSrc() throws Exception {
    String div = "<messageML><div data-icon-src=\"attr\">txt</div></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"data-icon-src\" is only allowed if the element class is \"card\".");
    context.parseMessageML(div, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDivInvalidAccentColor() throws Exception {
    String div = "<messageML><div data-accent-color=\"attr\">txt</div></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"data-accent-color\" is only allowed if the element class is \"card\".");
    context.parseMessageML(div, null, MessageML.MESSAGEML_VERSION);
  }
}
