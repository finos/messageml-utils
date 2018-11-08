package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.fasterxml.jackson.databind.JsonNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class CashtagTest extends ElementTest {

  @Test
  public void testCashTag() throws Exception {
    String input = "<messageML>Hello <cash tag=\"world\"/>!</messageML>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"keyword1\">$world</span>!"
        + "</div>";
    String expectedJson = "{\"keyword1\":{"
        + "\"type\":\"org.symphonyoss.fin.security\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.fin.security.id.ticker\","
        + "\"value\":\"world\""
        + "}]}}";
    String expectedText = "world";
    String expectedMarkdown = "Hello $world!";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element attributes", Collections.emptyMap(), messageML.getChildren().get(1).getAttributes());
    verifyCashTag(messageML, expectedPresentationML, expectedJson, expectedText, expectedMarkdown);
  }

  @Test
  public void testCashTagNonAlnum() throws Exception {
    String input = "<messageML>Hello <cash tag=\"_hello.w-o-r-l-d_\"/>!</messageML>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"keyword1\">$_hello.w-o-r-l-d_</span>!"
        + "</div>";
    String expectedJson = "{\"keyword1\":{"
        + "\"type\":\"org.symphonyoss.fin.security\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.fin.security.id.ticker\","
        + "\"value\":\"_hello.w-o-r-l-d_\""
        + "}]}}";
    String expectedText = "_hello.w-o-r-l-d_";
    String expectedMarkdown = "Hello $_hello.w-o-r-l-d_!";

    // Verify by MessageML
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    assertEquals("Element attributes", Collections.emptyMap(), messageML.getChildren().get(1).getAttributes());
    verifyCashTag(messageML, expectedPresentationML, expectedJson, expectedText, expectedMarkdown);

    // Verify by PresentationML
    context.parseMessageML(expectedPresentationML, expectedJson, MessageML.MESSAGEML_VERSION);
    messageML = context.getMessageML();
    verifyCashTag(messageML, expectedPresentationML, expectedJson, expectedText, expectedMarkdown);
  }

  @Test
  public void testCashTagMoreSpecialChars() throws Exception {
    String input = "<messageML>Hello <cash tag=\"_hello#w-o-r-l-d_\"/>!</messageML>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "Hello <span class=\"entity\" data-entity-id=\"keyword1\">$_hello#w-o-r-l-d_</span>!"
            + "</div>";
    String expectedJson = "{\"keyword1\":{"
            + "\"type\":\"org.symphonyoss.fin.security\","
            + "\"version\":\"1.0\","
            + "\"id\":[{"
            + "\"type\":\"org.symphonyoss.fin.security.id.ticker\","
            + "\"value\":\"_hello#w-o-r-l-d_\""
            + "}]}}";
    String expectedText = "_hello#w-o-r-l-d_";
    String expectedMarkdown = "Hello $_hello#w-o-r-l-d_!";

    // Verify by MessageML
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    assertEquals("Element attributes", Collections.emptyMap(), messageML.getChildren().get(1).getAttributes());
    verifyCashTag(messageML, expectedPresentationML, expectedJson, expectedText, expectedMarkdown);

    // Verify by PresentationML
    context.parseMessageML(expectedPresentationML, expectedJson, MessageML.MESSAGEML_VERSION);
    messageML = context.getMessageML();
    verifyCashTag(messageML, expectedPresentationML, expectedJson, expectedText, expectedMarkdown);
  }

  @Test
  public void testCashTagByPresentationMLDiv() throws Exception {

    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <div class=\"entity\" data-entity-id=\"cash123\">world</div>!"
        + "</div>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <div class=\"entity\" data-entity-id=\"cash123\">$world</div>!"
        + "</div>";
    String entityJson = "{\"cash123\":{"
        + "\"type\":\"org.symphonyoss.fin.security\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.fin.security.id.ticker\","
        + "\"value\":\"world\""
        + "}]}}";
    String expectedText = "world";
    String expectedMarkdown = "Hello $world!";

    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element attributes", 1, messageML.getChildren().get(1).getAttributes().size());
    assertEquals("Element class attribute", "entity", messageML.getChildren().get(1).getAttribute("class"));
    verifyCashTag(messageML, expectedPresentationML, entityJson, expectedText, expectedMarkdown);
  }

  @Test
  public void testCashTagByPresentationMLSpan() throws Exception {

    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"cash123\">world</span>!"
        + "</div>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"cash123\">$world</span>!"
        + "</div>";
    String entityJson = "{\"cash123\":{"
        + "\"type\":\"org.symphonyoss.fin.security\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.fin.security.id.ticker\","
        + "\"value\":\"world\""
        + "}]}}";
    String expectedText = "world";
    String expectedMarkdown = "Hello $world!";

    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element attributes", 1, messageML.getChildren().get(1).getAttributes().size());
    assertEquals("Element class attribute", "entity", messageML.getChildren().get(1).getAttribute("class"));
    verifyCashTag(messageML, expectedPresentationML, entityJson, expectedText, expectedMarkdown);
  }

  @Test
  public void testCashTagByPresentationMLMissingEntityId() throws Exception {
    String input = "<messageML>Hello <span class=\"entity\">world</span>!</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"data-entity-id\" is required");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCashTagInvalidCharacter() throws Exception {
    String input = "<messageML>Hello <cash tag=\"invalid chars!\"/></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(String.format("CashTag must match the following pattern : %s", CashTag.CASHTAG_PATTERN));
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCashTagByPresentationMLInvalidCharacter() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <div class=\"entity\" data-entity-id=\"cash123\">world</div>!"
        + "</div>";

    String entityJson = "{\"cash123\":{"
        + "\"type\":\"org.symphonyoss.fin.security\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.fin.security.id.ticker\","
        + "\"value\":\"invalid chars!\""
        + "}]}}";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(String.format("CashTag must match the following pattern : %s", CashTag.CASHTAG_PATTERN));
    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCashTagInvalidAttr() throws Exception {
    String invalidAttr = "<messageML>Hello <cash tag=\"world\" class=\"label\"/>!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"cash\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLShorthandCashTag() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\"><cash tag=\"invalid\"/></div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"cash\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }


  private void verifyCashTag(Element messageML, String expectedPresentationML, String expectedJson, String expectedText,
      String expectedMarkdown) throws Exception {
    assertEquals("Element children", 3, messageML.getChildren().size());

    Element cashtag = messageML.getChildren().get(1);

    assertEquals("Element class", CashTag.class, cashtag.getClass());
    assertEquals("Element tag name", "cash", cashtag.getMessageMLTag());
    assertEquals("Element text", expectedText, ((CashTag) cashtag).getTag());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("EntityJSON", expectedJson, MAPPER.writeValueAsString(context.getEntityJson()));
    assertEquals("Legacy entities", 1, context.getEntities().size());

    JsonNode entity = context.getEntities().get("hashtags");
    assertNotNull("Entity node", entity);
    assertEquals("Entity count", 1, entity.size());

    assertEquals("Entity text", "$" + expectedText, entity.get(0).get("text").textValue());
    assertEquals("Entity id", "$" + expectedText, entity.get(0).get("id").textValue());
    assertEquals("Entity type", "KEYWORD", entity.get(0).get("type").textValue());
  }
}
