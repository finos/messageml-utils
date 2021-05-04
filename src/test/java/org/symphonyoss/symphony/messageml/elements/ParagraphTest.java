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

public class ParagraphTest extends ElementTest {

  @Test
  public void testParagraph() throws Exception {
    String input = "<messageML>Hello<p>world</p>!</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 3, messageML.getChildren().size());

    Element p = messageML.getChildren().get(1);

    assertEquals("Element class", Paragraph.class, p.getClass());
    assertEquals("Element tag name", "p", p.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), p.getAttributes());
    assertEquals("Element children", 1, p.getChildren().size());
    assertEquals("Child element", TextNode.class, p.getChildren().get(0).getClass());
    assertEquals("Child element text", "world", p.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello<p>world</p>!</div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello\n\nworld\n\n!", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML>Hello<p class=\"label\">world</p>!</messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    p = context.getMessageML().getChildren().get(1);
    assertEquals("Attribute count", 1, p.getAttributes().size());
    assertEquals("Attribute", "label", p.getAttribute("class"));

    String styleAttr = "<messageML>Hello<p style=\"opacity:0.5\">world</p>!</messageML>";
    context.parseMessageML(styleAttr, null, MessageML.MESSAGEML_VERSION);
    p = context.getMessageML().getChildren().get(1);
    assertEquals("Attribute count", 1, p.getAttributes().size());
    assertEquals("Attribute", "opacity:0.5", p.getAttribute("style"));
  }

  @Test
  public void testParagraphInvalidAttr() throws Exception {
    String invalidAttr = "<messageML>Hello<p title=\"label\">world</p>!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"p\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testParagraphBi() throws Exception {
    String input = "<messageML><p>Hello</p><p>world</p>!</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    BiContext biContext = context.getBiContext();
    assertEquals(2, biContext.getItems().size());

    BiItem item = biContext.getItems().get(0);
    assertEquals(BiFields.PARAGRAPH.getFieldName(), item.getName());
    assertEquals(2, item.getAttributes().get(BiFields.COUNT.getFieldName()));
  }
}
