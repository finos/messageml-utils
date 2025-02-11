package org.finos.symphony.messageml.messagemlutils.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

import java.util.Collections;

public class HorizontalRuleTest extends ElementTest {

  @Test
  public void testHorizontalRule() throws Exception {
    String input = "<messageML>Hello<hr/>world!</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 3, messageML.getChildren().size());

    Element hr = messageML.getChildren().get(1);

    assertEquals("Element class", HorizontalRule.class, hr.getClass());
    assertEquals("Element tag name", "hr", hr.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), hr.getAttributes());
    assertEquals("Element children", Collections.<Element>emptyList(), hr.getChildren());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello<hr/>world!</div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello\n\n---\n\nworld!", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testHorizontalRuleInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><hr title=\"label\"/></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"hr\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testHorizontalRuleInvalidContent() throws Exception {
    String invalidContent = "<messageML><hr>Hello world!</hr></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"hr\" may not have child elements or text content");
    context.parseMessageML(invalidContent, null, MessageML.MESSAGEML_VERSION);
  }
}
