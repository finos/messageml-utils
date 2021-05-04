package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.TestDataProvider;
import org.symphonyoss.symphony.messageml.util.UserPresentation;

import java.util.Collections;

public class HeaderTest extends ElementTest {

  @Test
  public void testHeaders() throws Exception {
    for (int level = 1; level < 7; level++) {
      String input = "<messageML><h" + level + ">Hello world!</h" + level + "></messageML>";
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

      Element messageML = context.getMessageML();
      assertEquals("Element children", 1, messageML.getChildren().size());

      Element header = messageML.getChildren().get(0);

      assertEquals("Element class", Header.class, header.getClass());
      assertEquals("Element tag name", "h" + level, header.getMessageMLTag());
      assertEquals("Element attributes", Collections.emptyMap(), header.getAttributes());
      assertEquals("Element children", 1, header.getChildren().size());
      assertEquals("Child element", "Hello world!", header.getChildren().get(0).asText());
      assertEquals("PresentationML",
          "<div data-format=\"PresentationML\" data-version=\"2.0\"><h" + level + ">Hello world!</h" + level
              + "></div>",
          context.getPresentationML());
      assertEquals("Markdown", "**Hello world!**", context.getMarkdown());
      assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
      assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

      String withAttr = "<messageML><h" + level + " class=\"label\">Hello world!</h" + level + "></messageML>";
      context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
      header = context.getMessageML().getChildren().get(0);
      assertEquals("Attribute count", 1, header.getAttributes().size());
      assertEquals("Attribute", "label", header.getAttribute("class"));

      String styleAttr = "<messageML><h" + level + " style=\"height:300px\">Hello world!</h" + level + "></messageML>";
      context.parseMessageML(styleAttr, null, MessageML.MESSAGEML_VERSION);
      header = context.getMessageML().getChildren().get(0);
      assertEquals("Attribute count", 1, header.getAttributes().size());
      assertEquals("Attribute", "height:300px", header.getAttribute("style"));
    }
  }

  @Test
  public void testHeadersInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><h1 title=\"label\">Hello world!</h1></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"h1\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testHeadersInvalidTag() throws Exception {
    String invalidLevel = "<messageML><h7>Hello world!</h7></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid MessageML content at element \"h7\"");
    context.parseMessageML(invalidLevel, null, MessageML.MESSAGEML_VERSION);
    context.getMessageML();
  }

  @Test
  public void testHeadersInvalidContent() throws Exception {
    String invalidContent = "<messageML><h1><pre>>Hello world!</pre></h1></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"pre\" is not allowed in \"h1\"");
    context.parseMessageML(invalidContent, null, MessageML.MESSAGEML_VERSION);
    context.getMessageML();
  }

  @Test
  public void testHeaderWithMention() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String input = "<messageML><h1>Hello <mention email=\"bot.user1@localhost.com\"/>!</h1></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "<h1>Hello <span class=\"entity\" data-entity-id=\"mention1\">@Bot User01</span>!</h1></div>",
        context.getPresentationML());
    assertEquals("Markdown", "**Hello @Bot User01!**", context.getMarkdown());
    assertEquals("Plaintext", "Hello @Bot User01!", context.getText());
  }

  @Test
  public void testHeaderBi() throws Exception {
    String input = "<messageML>" +
        "<h1>Big title</h1>" +
        "<h4>Subtitle</h4>" +
        "<h4>Another subtitle</h4>" +
        "<h6>text</h6>" +
        "</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    BiContext biContext = context.getBiContext();
    assertEquals(2, biContext.getItems().size());

    BiItem item = biContext.getItems().get(0);
    assertEquals(BiFields.HEADER.getFieldName(), item.getName());
    assertEquals(4, item.getAttributes().get(BiFields.COUNT.getFieldName()));
  }
}
