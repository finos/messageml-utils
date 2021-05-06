package org.symphonyoss.symphony.messageml.elements;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.util.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

public class MentionTest extends ElementTest {

  @Test
  public void testMentionByUid() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String input = "<messageML>Hello <mention uid=\"1\"/>!</messageML>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"mention1\">@Bot User01</span>!"
        + "</div>";
    String expectedJson = "{\"mention1\":{"
        + "\"type\":\"com.symphony.user.mention\","
        + "\"version\":\"1.0\",\"id\":[{"
        + "\"type\":\"com.symphony.user.userId\","
        + "\"value\":\"1\""
        + "}]}}";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element attributes", Collections.emptyMap(), messageML.getChildren().get(1).getAttributes());
    verifyMention(messageML, user, expectedPresentationML, expectedJson);
  }

  @Test
  public void testMentionByEmail() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String input = "<messageML>Hello <mention email=\"bot.user1@localhost.com\"/>!</messageML>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"mention1\">@Bot User01</span>!"
        + "</div>";
    String expectedJson = "{\"mention1\":{"
        + "\"type\":\"com.symphony.user.mention\","
        + "\"version\":\"1.0\",\"id\":[{"
        + "\"type\":\"com.symphony.user.userId\","
        + "\"value\":\"1\""
        + "}]}}";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element attributes", Collections.emptyMap(), messageML.getChildren().get(1).getAttributes());
    verifyMention(messageML, user, expectedPresentationML, expectedJson);
  }

  @Test
  public void testMentionWithPrettyName() throws Exception {
    UserPresentation user =
        new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String input =
        "<messageML>Hello <mention email=\"bot.user1@localhost.com\">This will be "
            + "ignored</mention>!</messageML>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"mention1\">@Bot User01</span>!"
        + "</div>";
    String expectedJson = "{\"mention1\":{"
        + "\"type\":\"com.symphony.user.mention\","
        + "\"version\":\"1.0\",\"id\":[{"
        + "\"type\":\"com.symphony.user.userId\","
        + "\"value\":\"1\""
        + "}]}}";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element attributes", Collections.emptyMap(),
        messageML.getChildren().get(1).getAttributes());
    verifyMention(messageML, user, expectedPresentationML, expectedJson);
  }

  @Test
  public void testMentionByPresentationMLDiv() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <div class=\"entity\" data-entity-id=\"mention123\">@Bot User01</div>!"
        + "</div>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <div class=\"entity\" data-entity-id=\"mention123\">@Bot User01</div>!"
        + "</div>";
    String entityJson = "{\"mention123\":{"
        + "\"type\":\"com.symphony.user.mention\","
        + "\"version\":\"1.0\",\"id\":[{"
        + "\"type\":\"com.symphony.user.userId\","
        + "\"value\":1"
        + "}"
        + "]}}";

    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element attributes", 1, messageML.getChildren().get(1).getAttributes().size());
    assertEquals("Element class attribute", "entity", messageML.getChildren().get(1).getAttribute("class"));
    verifyMention(messageML, user, expectedPresentationML, entityJson);
  }

  @Test
  public void testMentionByPresentationMLSpan() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"mention123\">@Bot User01</span>!"
        + "</div>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"mention123\">@Bot User01</span>!"
        + "</div>";
    String entityJson = "{\"mention123\":{"
        + "\"type\":\"com.symphony.user.mention\","
        + "\"version\":\"1.0\",\"id\":[{"
        + "\"type\":\"com.symphony.user.userId\","
        + "\"value\":1"
        + "}"
        + "]}}";

    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element attributes", 1, messageML.getChildren().get(1).getAttributes().size());
    assertEquals("Element class attribute", "entity", messageML.getChildren().get(1).getAttribute("class"));
    verifyMention(messageML, user, expectedPresentationML, entityJson);
  }

  @Test
  public void testMentionByPresentationMLInvalidUser() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String input = "<messageML>Hello "
        + "<span class=\"entity\" data-entity-id=\"mention1\">@Invalid user</span>"
        + "!</messageML>";

    String entityJson = "{\"mention1\":{"
        + "\"type\":\"com.symphony.user.mention\","
        + "\"version\":\"1.0\",\"id\":[{"
        + "\"type\":\"com.symphony.user.userId\","
        + "\"value\":0"
        + "}"
        + "]}}";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Failed to lookup user \"0\"");
    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testMentionInvalidAttr() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String invalidAttr = "<messageML>Hello <mention uid=\"1\" class=\"label\"/>!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"mention\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testHardMentionInvalidEmail() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String invalidAttr = "<messageML>Hello <mention email=\"invalid@email.com\"/>!</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Failed to lookup user \"invalid@email.com\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSoftMentionInvalidEmail() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String invalidAttr = "<messageML>Hello <mention email=\"invalid@email.com\" strict=\"false\" />!</messageML>";
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "Hello <a href=\"mailto:invalid@email.com\">invalid@email.com</a>!</div>",
        context.getPresentationML());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Markdown", "Hello invalid@email.com!", context.getMarkdown());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testSoftMentionWithPrettyNameInvalidEmail() throws Exception {
    UserPresentation user =
        new UserPresentation(0L, "never.retrieved", "Never retrieved", "ignored@email.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String invalidAttr = "<messageML>Hello <mention email=\"invalid@email.com\" strict=\"false\">"
        + "Bot User01</mention>!</messageML>";
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "Hello <a href=\"mailto:invalid@email.com\">Bot User01</a>!</div>",
        context.getPresentationML());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Markdown", "Hello invalid@email.com!", context.getMarkdown());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testHardMentionInvalidUid() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String invalidAttr = "<messageML>Hello <mention uid=\"0\"/>!</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Failed to lookup user \"0\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSoftMentionInvalidUid() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String invalidAttr = "<messageML>Hello <mention uid=\"0\" strict=\"false\" />!</messageML>";
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);

    JsonNode expectedJson = MAPPER.readTree("{\"mention1\":{\"type\":\"com.symphony.user.mention\",\"version\":\"1.0\","
        + "\"id\":[{\"type\":\"com.symphony.user.userId\",\"value\":\"0\"}]}}");

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "Hello <span class=\"entity\" data-entity-id=\"mention1\">0</span>!</div>",
        context.getPresentationML());
    assertEquals("EntityJSON", expectedJson, context.getEntityJson());
    assertEquals("Markdown", "Hello 0!", context.getMarkdown());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testMentionMalformedUid() throws Exception {
    String invalidAttr = "<messageML>Hello <mention uid=\"bot.user1\"/>!</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid input: uid must be a int64 value not \"bot.user1\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testMentionByMarkdown() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String markdown = "Hello @Bot User01!";
    JsonNode entities = MAPPER.readTree("{\"userMentions\": [{"
        + "        \"id\": 1,"
        + "        \"screenName\": \"bot.user1\","
        + "        \"prettyName\": \"Bot User02\","
        + "        \"text\": \"@Bot User01\","
        + "        \"indexStart\": 6,"
        + "        \"indexEnd\": 17,"
        + "        \"userType\": \"lc\","
        + "        \"type\": \"USER_FOLLOW\""
        + "    }]"
        + "}");

    context.parseMarkdown(markdown, entities, null);

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"mention1\">@Bot User01</span>!</div>";
    JsonNode expectedEntityJSON =
        MAPPER.readTree("{\"mention1\":{\"type\":\"com.symphony.user.mention\",\"version\":\"1.0\","
            + "\"id\":[{\"type\":\"com.symphony.user.userId\",\"value\":\"1\"}]}}");

    String presentationML = context.getPresentationML();
    JsonNode entityJson = context.getEntityJson();
    assertEquals("Generated PresentationML", expectedPresentationML, presentationML);
    assertEquals("Generated EntityJSON", expectedEntityJSON, entityJson);
  }

  @Test
  public void testMentionByMarkdownInvalidUser() throws Exception {
    IDataProvider mockDataProvider = mock(IDataProvider.class);
    doThrow(new InvalidInputException("Expected")).when(mockDataProvider).getUserPresentation(1L);
    MessageMLContext mockContext = spy(new MessageMLContext(mockDataProvider));

    String markdown = "Hello @Bot User01!";
    JsonNode entities = MAPPER.readTree("{\"userMentions\": [{"
        + "        \"id\": 1,"
        + "        \"screenName\": \"bot.user1\","
        + "        \"prettyName\": \"Bot User02\","
        + "        \"text\": \"@Bot User01\","
        + "        \"indexStart\": 6,"
        + "        \"indexEnd\": 17,"
        + "        \"userType\": \"lc\","
        + "        \"type\": \"USER_FOLLOW\""
        + "    }]"
        + "}");

    mockContext.parseMarkdown(markdown, entities, null);

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"mention1\">1</span>!</div>";
    JsonNode expectedEntityJSON =
        MAPPER.readTree("{\"mention1\":{\"type\":\"com.symphony.user.mention\",\"version\":\"1.0\","
            + "\"id\":[{\"type\":\"com.symphony.user.userId\",\"value\":\"1\"}]}}");
    String expectedText = "Hello 1!";

    String presentationML = mockContext.getPresentationML();
    JsonNode entityJson = mockContext.getEntityJson();
    assertEquals("Generated PresentationML", expectedPresentationML, presentationML);
    assertEquals("Generated EntityJSON", expectedEntityJSON, entityJson);
    assertEquals("Geerated Markdown", expectedText, mockContext.getMarkdown());
    assertEquals("Generated text", expectedText, mockContext.getText());
  }

  @Test
  public void testPresentationMLShorthandMention() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\"><mention uid=\"1\"/></div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"mention\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testNullDataProviderMentionByEmail() throws Exception {
    String input = "<messageML>Hello <mention email=\"bot.user1@localhost.com\"/>!</messageML>";

    IDataProvider nullProvider = new NullDataProvider();
    MessageMLContext context = new MessageMLContext(nullProvider);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();
    ObjectNode entityJson = context.getEntityJson();
    String markdown = context.getMarkdown();
    String text = context.getText();

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "Hello <span class=\"entity\" data-entity-id=\"mention1\"></span>!" +
        "</div>";
    String expectedJson =
        "{\"mention1\":{\"type\":\"com.symphony.user.mention\",\"version\":\"1.0\",\"id\":[{\"type\":\"com.symphony.user.userId\",\"value\":\"0\"}]}}";
    String expectedMarkdown = "Hello !";
    String expectedText = "Hello !";

    assertEquals("Generated PresentationML", expectedPresentationML, presentationML);
    assertEquals("Generated EntityJSON", expectedJson, MAPPER.writeValueAsString(entityJson));
    assertEquals("Generated Markdown", expectedMarkdown, markdown);
    assertEquals("Generated text", expectedText, text);
  }

  @Test
  public void testNullDataProviderMentionByUid() throws Exception {
    String input = "<messageML>Hello <mention uid=\"1000\"/>!</messageML>";

    IDataProvider nullProvider = new NullDataProvider();
    MessageMLContext context = new MessageMLContext(nullProvider);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();
    ObjectNode entityJson = context.getEntityJson();
    String markdown = context.getMarkdown();
    String text = context.getText();

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "Hello <span class=\"entity\" data-entity-id=\"mention1\"></span>!" +
        "</div>";
    String expectedJson =
        "{\"mention1\":{\"type\":\"com.symphony.user.mention\",\"version\":\"1.0\",\"id\":[{\"type\":\"com.symphony.user.userId\",\"value\":\"1000\"}]}}";
    String expectedMarkdown = "Hello !";
    String expectedText = "Hello !";

    assertEquals("Generated PresentationML", expectedPresentationML, presentationML);
    assertEquals("Generated EntityJSON", expectedJson, MAPPER.writeValueAsString(entityJson));
    assertEquals("Generated Markdown", expectedMarkdown, markdown);
    assertEquals("Generated text", expectedText, text);
  }

  @Test
  public void testNoOpDataProviderMentionByEmail() throws Exception {
    String input = "<messageML>Hello <mention email=\"bot.user1@localhost.com\"/>!</messageML>";

    IDataProvider noOpProvider = new NoOpDataProvider();
    MessageMLContext context = new MessageMLContext(noOpProvider);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();
    ObjectNode entityJson = context.getEntityJson();
    String markdown = context.getMarkdown();
    String text = context.getText();

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "Hello <span class=\"entity\" data-entity-id=\"mention1\">@bot.user1@localhost.com</span>!" +
        "</div>";
    String expectedJson =
        "{\"mention1\":{\"type\":\"com.symphony.user.mention\",\"version\":\"1.0\",\"id\":[{\"type\":\"com.symphony.user.userId\",\"value\":\"0\"}]}}";
    String expectedMarkdown = "Hello @bot.user1@localhost.com!";
    String expectedText = "Hello @bot.user1@localhost.com!";

    assertEquals("Generated PresentationML", expectedPresentationML, presentationML);
    assertEquals("Generated EntityJSON", expectedJson, MAPPER.writeValueAsString(entityJson));
    assertEquals("Generated Markdown", expectedMarkdown, markdown);
    assertEquals("Generated text", expectedText, text);
  }

  @Test
  public void testNoOpDataProviderMentionByUid() throws Exception {
    String input = "<messageML>Hello <mention uid=\"1000\"/>!</messageML>";

    IDataProvider noOpProvider = new NoOpDataProvider();
    MessageMLContext context = new MessageMLContext(noOpProvider);

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();
    ObjectNode entityJson = context.getEntityJson();
    String markdown = context.getMarkdown();
    String text = context.getText();

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">" +
        "Hello <span class=\"entity\" data-entity-id=\"mention1\">@1000</span>!" +
        "</div>";
    String expectedJson =
        "{\"mention1\":{\"type\":\"com.symphony.user.mention\",\"version\":\"1.0\",\"id\":[{\"type\":\"com.symphony.user.userId\",\"value\":\"1000\"}]}}";
    String expectedMarkdown = "Hello @1000!";
    String expectedText = "Hello @1000!";

    assertEquals("Generated PresentationML", expectedPresentationML, presentationML);
    assertEquals("Generated EntityJSON", expectedJson, MAPPER.writeValueAsString(entityJson));
    assertEquals("Generated Markdown", expectedMarkdown, markdown);
    assertEquals("Generated text", expectedText, text);
  }

  @Test
  public void testMentionBi() throws Exception {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);
    String input = "<messageML>Hello <mention uid=\"1\"/>, <mention email=\"bot.user1@localhost.com\"/>!</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    List<BiItem> expectedBiItems = getExpectedMentionBiItems();

    List<BiItem> biItems = context.getBiContext().getItems();
    assertEquals(biItems.size(), expectedBiItems.size());
    assertTrue(biItems.containsAll(expectedBiItems));
    assertTrue(expectedBiItems.containsAll(biItems));
  }

  @Test
  public void testBiContextMentionEntity() throws InvalidInputException, IOException,
      ProcessingException {
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    ((TestDataProvider) dataProvider).setUserPresentation(user);

    String input = "<messageML>Hello <mention uid=\"1\"/>!</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    List<BiItem> items = context.getBiContext().getItems();

    Map<String, Object> mentionExpectedAttributes =
        Collections.singletonMap(BiFields.COUNT.getValue(), 1);
    Map<String, Object> entityExpectedAttributes =
        Collections.singletonMap(BiFields.ENTITY_TYPE.getValue(), "com.symphony.user.userId");

    BiItem mentionBiItemExpected = new BiItem(BiFields.MENTIONS.getValue(), mentionExpectedAttributes);
    BiItem entityBiItemExpected = new BiItem(BiFields.ENTITY.getValue(), entityExpectedAttributes);

    assertEquals(3, items.size());
    assertSameBiItem(entityBiItemExpected, items.get(0));
    assertSameBiItem(mentionBiItemExpected, items.get(1));
    assertMessageLengthBiItem(items.get(2), input.length());
  }

  private List<BiItem> getExpectedMentionBiItems() {
    List<BiItem> biItems = new ArrayList<>();
    biItems.add(new BiItem(BiFields.MENTIONS.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 2)));
    biItems.add(new BiItem(BiFields.ENTITY.getValue(), Collections.singletonMap(BiFields.ENTITY_TYPE.getValue(), "com.symphony.user.userId")));
    biItems.add(new BiItem(BiFields.ENTITY.getValue(), Collections.singletonMap(BiFields.ENTITY_TYPE.getValue(), "com.symphony.user.userId")));
    biItems.add(new BiItem(BiFields.MESSAGE_LENGTH.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 92)));
    return biItems;
  }


  private void verifyMention(Element messageML, UserPresentation user, String expectedPresentationML,
      String expectedJson)
      throws Exception {
    assertEquals("Element children", 3, messageML.getChildren().size());

    Element mention = messageML.getChildren().get(1);

    assertEquals("Element class", Mention.class, mention.getClass());
    assertEquals("Element tag name", "mention", mention.getMessageMLTag());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", "Hello @Bot User01!", context.getMarkdown());
    assertEquals("EntityJSON", expectedJson, MAPPER.writeValueAsString(context.getEntityJson()));
    assertEquals("Legacy entities", 1, context.getEntities().size());

    JsonNode entity = context.getEntities().get("userMentions");
    assertNotNull("Entity node", entity);
    assertEquals("Entity count", 1, entity.size());

    assertEquals("Entity text", Mention.PREFIX + user.getPrettyName(), entity.get(0).get("text").textValue());
    assertEquals("Entity id", user.getId(), entity.get(0).get("id").longValue());
    assertEquals("Entity user screen name", user.getScreenName(), entity.get(0).get("screenName").textValue());
    assertEquals("Entity user pretty name", user.getPrettyName(), entity.get(0).get("prettyName").textValue());
    assertEquals("Entity user type", "lc", entity.get(0).get("userType").textValue());
    assertEquals("Entity start index", 6, entity.get(0).get("indexStart").intValue());
    assertEquals("Entity end index", 17, entity.get(0).get("indexEnd").intValue());
    assertEquals("Entity type", "USER_FOLLOW", entity.get(0).get("type").textValue());
  }

}
