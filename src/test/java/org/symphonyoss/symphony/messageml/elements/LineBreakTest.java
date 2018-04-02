package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class LineBreakTest extends ElementTest {

  @Test
  public void testLineBreak() throws Exception {
    String input = "<messageML>Hello<br/>world!</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 3, messageML.getChildren().size());

    Element br = messageML.getChildren().get(1);

    assertEquals("Element class", LineBreak.class, br.getClass());
    assertEquals("Element tag name", "br", br.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), br.getAttributes());
    assertEquals("Element children", Collections.<Element>emptyList(), br.getChildren());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello<br/>world!</div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello\nworld!", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testLineBreakInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><br title=\"label\"/></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"br\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testLineBreakInvalidContent() throws Exception {
    String invalidContent = "<messageML><br>Hello world!</br></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"br\" may not have child elements or text content");
    context.parseMessageML(invalidContent, null, MessageML.MESSAGEML_VERSION);
  }
}
