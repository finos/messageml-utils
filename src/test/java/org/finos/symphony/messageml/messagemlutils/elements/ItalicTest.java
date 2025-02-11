package org.finos.symphony.messageml.messagemlutils.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

import java.util.Collections;

public class ItalicTest extends ElementTest {

  @Test
  public void testItalic() throws Exception {
    String input = "<messageML><i>Hello world!</i></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element italic = messageML.getChildren().get(0);

    assertEquals("Element class", Italic.class, italic.getClass());
    assertEquals("Element tag name", "i", italic.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), italic.getAttributes());
    assertEquals("Element children", 1, italic.getChildren().size());
    assertEquals("Child element", TextNode.class, italic.getChildren().get(0).getClass());
    assertEquals("Child element text", "Hello world!", italic.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\"><i>Hello world!</i></div>",
        context.getPresentationML());
    assertEquals("Markdown", "_Hello world!_", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><i class=\"label\">Hello world!</i></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    italic = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, italic.getAttributes().size());
    assertEquals("Attribute", "label", italic.getAttribute("class"));

    String styleAttr = "<messageML><i style=\"background:red\">Hello world!</i></messageML>";
    context.parseMessageML(styleAttr, null, MessageML.MESSAGEML_VERSION);
    italic = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, italic.getAttributes().size());
    assertEquals("Attribute", "background:red", italic.getAttribute("style"));
  }

  @Test
  public void testItalicInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><i title=\"label\">Hello world!</i></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"i\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testBoldWithShorthandHashtag() throws Exception {
    String input = "<messageML><b>Hello <hash tag=\"world\"/>!</b></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "<b>Hello <span class=\"entity\" data-entity-id=\"keyword1\">#world</span>!</b></div>",
        context.getPresentationML());
    assertEquals("Markdown", "**Hello #world!**", context.getMarkdown());
    assertEquals("Plaintext", "Hello #world!", context.getText());
  }

  @Test
  public void testItalicWithShorthandCashtag() throws Exception {
    String input = "<messageML><i>Hello <cash tag=\"world\"/>!</i></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "<i>Hello <span class=\"entity\" data-entity-id=\"keyword1\">$world</span>!</i></div>",
        context.getPresentationML());
    assertEquals("Markdown", "_Hello $world!_", context.getMarkdown());
    assertEquals("Plaintext", "Hello $world!", context.getText());
  }
}
