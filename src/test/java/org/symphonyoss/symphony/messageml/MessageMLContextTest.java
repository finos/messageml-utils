/*
 * Copyright 2016-2017 MessageML - Symphony LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.symphonyoss.symphony.messageml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.symphonyoss.symphony.messageml.elements.BulletList;
import org.symphonyoss.symphony.messageml.elements.CashTag;
import org.symphonyoss.symphony.messageml.elements.Div;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.HashTag;
import org.symphonyoss.symphony.messageml.elements.Link;
import org.symphonyoss.symphony.messageml.elements.Mention;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.Paragraph;
import org.symphonyoss.symphony.messageml.elements.TextNode;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.UserPresentation;

import java.io.IOException;
import java.net.URI;
import java.util.List;
import java.util.Scanner;

import javax.xml.parsers.ParserConfigurationException;

public class MessageMLContextTest {

  private static final ObjectMapper MAPPER = new ObjectMapper();

  private final IDataProvider dataProvider = mock(IDataProvider.class);

  @Rule
  public final ExpectedException expectedException = ExpectedException.none();

  private MessageMLContext context;

  @Before
  public void setUp() throws InvalidInputException, ProcessingException, ParserConfigurationException {
    UserPresentation user = new UserPresentation(123456789L, "bot.user1", "Bot User01", "bot.user1@localhost.com");
    when(dataProvider.getUserPresentation(anyLong())).thenReturn(user);
    when(dataProvider.getUserPresentation(anyString())).thenReturn(user);
    context = new MessageMLContext(dataProvider);
  }

  private void validateMessageML(String expectedPresentationML, JsonNode expectedEntityJson,
      String expectedMarkdown, JsonNode expectedEntities) throws Exception {

    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("EntityJSON", MAPPER.writeValueAsString(expectedEntityJson),
        MAPPER.writeValueAsString(context.getEntityJson()));
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());

    JsonNode entities = context.getEntities();
    assertEquals("Entity URL id", expectedEntities.get("urls").get(0).get("id"),
        entities.get("urls").get(0).get("id"));
    assertEquals("Entity URL text", expectedEntities.get("urls").get(0).get("text"),
        entities.get("urls").get(0).get("text"));
    assertEquals("Entity URL url", expectedEntities.get("urls").get(0).get("expandedUrl"),
        entities.get("urls").get(0).get("expandedUrl"));
    assertEquals("Entity URL start index", expectedEntities.get("urls").get(0).get("indexStart"),
        entities.get("urls").get(0).get("indexStart"));
    assertEquals("Entity URL end index", expectedEntities.get("urls").get(0).get("indexEnd"),
        entities.get("urls").get(0).get("indexEnd"));
    assertEquals("Entity URL type", expectedEntities.get("urls").get(0).get("type"),
        entities.get("urls").get(0).get("type"));

    assertEquals("Entity user mention id", expectedEntities.get("userMentions").get(0).get("id").longValue(),
        entities.get("userMentions").get(0).get("id").longValue());
    assertEquals("Entity user mention screen name", expectedEntities.get("userMentions").get(0).get("screenName"),
        entities.get("userMentions").get(0).get("screenName"));
    assertEquals("Entity user mention pretty name", expectedEntities.get("userMentions").get(0).get("prettyName"),
        entities.get("userMentions").get(0).get("prettyName"));
    assertEquals("Entity user mention text", expectedEntities.get("userMentions").get(0).get("text"),
        entities.get("userMentions").get(0).get("text"));
    assertEquals("Entity user mention start index", expectedEntities.get("userMentions").get(0).get("indexStart"),
        entities.get("userMentions").get(0).get("indexStart"));
    assertEquals("Entity user mention end index", expectedEntities.get("userMentions").get(0).get("indexEnd"),
        entities.get("userMentions").get(0).get("indexEnd"));
    assertEquals("Entity user mention text", expectedEntities.get("userMentions").get(0).get("userType"),
        entities.get("userMentions").get(0).get("userType"));
    assertEquals("Entity user mention type", expectedEntities.get("userMentions").get(0).get("type"),
        entities.get("userMentions").get(0).get("type"));

    for (int i = 0; i < 2; i++) {
      assertEquals("Entity hashtag id", expectedEntities.get("hashtags").get(i).get("id"),
          entities.get("hashtags").get(i).get("id"));
      assertEquals("Entity hashtag text", expectedEntities.get("hashtags").get(i).get("text"),
          entities.get("hashtags").get(i).get("text"));
      assertEquals("Entity hashtag start index", expectedEntities.get("hashtags").get(i).get("indexStart"),
          entities.get("hashtags").get(i).get("indexStart"));
      assertEquals("Entity hashtag end index", expectedEntities.get("hashtags").get(i).get("indexEnd"),
          entities.get("hashtags").get(i).get("indexEnd"));
      assertEquals("Entity hashtag type", expectedEntities.get("hashtags").get(i).get("type"),
          entities.get("hashtags").get(i).get("type"));
    }
  }

  @Test
  public void testParseMessageML() throws Exception {
    String message = getPayload("payloads/templated_message_all_tags.messageml");
    String data = getPayload("payloads/templated_message_all_tags.json");

    final String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\"> "
        + "<p><img src=\"https://symphony.com/images/web/logo/symphony-logo-nav-light.svg\"/> <br/> "
        + "Sample JIRA issue</p> "
        + "<div class=\"entity\" data-entity-id=\"jiraUpdated\"> "
        + "<h1>Bot User01 updated Bug "
        + "<a href=\"https://whiteam1.atlassian.net/browse/SAM-24\"> "
        + "<i>SAM-24</i>,<b>Sample Bug Blocker</b> "
        + "</a> "
        + "</h1> "
        + "<div class=\"card barStyle\" "
        + "data-icon-src=\"https://symphony.com/images/web/logo/symphony-logo-nav-light.svg\"> "
        + "<div class=\"cardHeader\"> "
        + "<span class=\"label\">Field</span><span class=\"info\">Old Value =&gt; New Value</span> "
        + "</div> "
        + "<div class=\"cardBody\"> "
        + "<ol> "
        + "<li> "
        + "<span class=\"label\">resolution</span> "
        + "<span class=\"info\">Open =&gt; Done</span> "
        + "</li> "
        + "<li> "
        + "<span class=\"label\">status</span> "
        + "<span class=\"info\">To Do =&gt; Done</span> </li> "
        + "</ol> "
        + "</div> "
        + "</div> "
        + "<hr/> "
        + "<table> "
        + "<thead> "
        + "<tr> "
        + "<th>Field</th> "
        + "<th>Value</th> "
        + "</tr> "
        + "</thead> "
        + "<tbody> "
        + "<tr> "
        + "<td>Assignee</td> "
        + "<td><span class=\"entity\" data-entity-id=\"mention1\">@Bot User01</span></td> "
        + "</tr> "
        + "<tr> "
        + "<td>Labels</td> "
        + "<td> "
        + "<ul> "
        + "<li><span class=\"entity\" data-entity-id=\"keyword2\">#production</span></li> "
        + "<li><span class=\"entity\" data-entity-id=\"keyword3\">#major</span></li> "
        + "</ul> "
        + "</td> "
        + "</tr> "
        + "</tbody> "
        + "<tfoot> "
        + "<tr> "
        + "<th>Priority</th> "
        + "<td>Highest</td> "
        + "</tr> "
        + "<tr> "
        + "<th>Status</th> "
        + "<td>Done</td> "
        + "</tr> "
        + "</tfoot> "
        + "</table> "
        + "</div> "
        + "</div>";
    final String expectedMarkdown = " \n"
        + "\n"
        + " \n"
        + " Sample JIRA issue\n"
        + "\n"
        + " \n"
        + "\n"
        + " **Bot User01 updated Bug https://whiteam1.atlassian.net/browse/SAM-24 **  \n"
        + "\n"
        + " FieldOld Value => New Value \n"
        + "\n"
        + " \n"
        + "\n"
        + " \n"
        + " 1.  resolution Open => Done \n"
        + " 2.  status To Do => Done \n"
        + " \n"
        + " \n"
        + "\n"
        + "  \n"
        + "\n"
        + "---\n"
        + "\n"
        + " Table:\n"
        + "---\n"
        + "   Field |  Value |  \n"
        + "    Assignee |  @Bot User01 |  \n"
        + "  Labels |   \n"
        + " - #production\n"
        + " - #major\n"
        + " \n"
        + "  |  \n"
        + "    Priority |  Highest |  \n"
        + "  Status |  Done |  \n"
        + "  \n"
        + "---\n"
        + " \n"
        + "\n"
        + " ";
    final JsonNode expectedEntities = MAPPER.readTree("{\n"
        + "    \"urls\": [{\n"
        + "        \"text\": \"https://whiteam1.atlassian.net/browse/SAM-24\",\n"
        + "        \"id\": \"https://whiteam1.atlassian.net/browse/SAM-24\",\n"
        + "        \"expandedUrl\": \"https://whiteam1.atlassian.net/browse/SAM-24\",\n"
        + "        \"indexStart\": 54,\n"
        + "        \"indexEnd\": 98,\n"
        + "        \"type\": \"URL\"\n"
        + "    }],\n"
        + "    \"userMentions\": [{\n"
        + "        \"id\": 123456789,\n"
        + "        \"screenName\": \"bot.user1\",\n"
        + "        \"prettyName\": \"Bot User01\",\n"
        + "        \"text\": \"@Bot User01\",\n"
        + "        \"indexStart\": 262,\n"
        + "        \"indexEnd\": 273,\n"
        + "        \"userType\": \"lc\",\n"
        + "        \"type\": \"USER_FOLLOW\"\n"
        + "    }],\n"
        + "    \"hashtags\": [{\n"
        + "        \"id\": \"#production\",\n"
        + "        \"text\": \"#production\",\n"
        + "        \"indexStart\": 295,\n"
        + "        \"indexEnd\": 306,\n"
        + "        \"type\": \"KEYWORD\"\n"
        + "    }, {\n"
        + "        \"id\": \"#major\",\n"
        + "        \"text\": \"#major\",\n"
        + "        \"indexStart\": 310,\n"
        + "        \"indexEnd\": 316,\n"
        + "        \"type\": \"KEYWORD\"\n"
        + "    }]\n"
        + "}");
    final ObjectNode expectedEntityJson = (ObjectNode) MAPPER.readTree(data);
    final String generatedEntities = "{\"mention1\":{"
        + "\"type\":\"com.symphony.user.mention\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"com.symphony.user.userId\","
        + "\"value\":\"123456789\""
        + "}]},"
        + "\"keyword2\":{"
        + "\"type\":\"org.symphonyoss.taxonomy\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.taxonomy.hashtag\","
        + "\"value\":\"production\""
        + "}]},"
        + "\"keyword3\":{"
        + "\"type\":\"org.symphonyoss.taxonomy\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.taxonomy.hashtag\","
        + "\"value\":\"major\""
        + "}]}}";
    ((ObjectNode) expectedEntityJson.get("jiraUpdated")).setAll((ObjectNode) MAPPER.readTree(generatedEntities));

    context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    assertNotNull("MessageML", messageML);
    assertEquals("Chime", false, messageML.isChime());

    List<Element> children = messageML.getChildren();
    assertEquals("MessageML children", 5, children.size());
    assertEquals("Child #1 class", TextNode.class, children.get(0).getClass());
    assertEquals("Child #1 text", "\n", ((TextNode) children.get(0)).getText());
    assertTrue("Child #1 attributes", children.get(0).getAttributes().isEmpty());
    assertTrue("Child #1 children", children.get(0).getChildren().isEmpty());

    assertEquals("Child #2 class", Paragraph.class, children.get(1).getClass());
    assertTrue("Child #2 attributes", children.get(1).getAttributes().isEmpty());
    assertEquals("Child #2 children", 4, children.get(1).getChildren().size());

    assertEquals("Child #3 class", TextNode.class, children.get(2).getClass());
    assertEquals("Child #3 text", "\n", ((TextNode) children.get(2)).getText());
    assertTrue("Child #3 attributes", children.get(2).getAttributes().isEmpty());
    assertTrue("Child #3 children", children.get(2).getChildren().isEmpty());

    assertEquals("Child #4 class", Div.class, children.get(3).getClass());
    assertEquals("Child #4 attributes", 2, children.get(3).getAttributes().size());
    assertEquals("Child #4 attribute", "entity", children.get(3).getAttribute("class"));
    assertEquals("Child #4 attribute", "jiraUpdated", children.get(3).getAttribute("data-entity-id"));
    assertEquals("Child #4 children", 9, children.get(3).getChildren().size());

    assertEquals("Child #5 class", TextNode.class, children.get(4).getClass());
    assertEquals("Child #5 text", "\n", ((TextNode) children.get(4)).getText());
    assertTrue("Child #5 attributes", children.get(4).getAttributes().isEmpty());
    assertTrue("Child #5 children", children.get(4).getChildren().isEmpty());

    validateMessageML(expectedPresentationML, expectedEntityJson, expectedMarkdown, expectedEntities);
  }

  @Test
  public void testParseEmptyMessage() throws Exception {
    context.parseMarkdown("", new ObjectNode(JsonNodeFactory.instance), null);

    MessageML messageML = context.getMessageML();
    assertTrue("Message children", messageML.getChildren().isEmpty());
    assertTrue("Message attributes", messageML.getAttributes().isEmpty());
  }

  @Test
  public void testParseMessageUnescapeChars() throws Exception {
    String mml = "<messageML>&lt;b&gt;Hello&lt;/b&gt; &lt;i&gt;world!&lt;/i&gt;</messageML>";
    String pml = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "&lt;b&gt;Hello&lt;/b&gt; &lt;i&gt;world!&lt;/i&gt;</div>";
    String md = "<b>Hello</b> <i>world!</i>";

    context.parseMessageML(mml, null, MessageML.MESSAGEML_VERSION);
    assertEquals("Expected PresentationML", pml, context.getPresentationML());
    assertEquals("Expected Markdown", md, context.getMarkdown());

    context.parseMessageML(pml, null, MessageML.MESSAGEML_VERSION);
    assertEquals("Expected PresentationML", pml, context.getPresentationML());
    assertEquals("Expected Markdown", md, context.getMarkdown());

    context.parseMarkdown(md, null, null);
    assertEquals("Expected PresentationML", pml, context.getPresentationML());
    assertEquals("Expected Markdown", md, context.getMarkdown());
  }

  @Test
  public void testParseFreemarker() throws Exception {
    String message = "<messageML>${data['obj123'].value}</messageML>";
    String data = "{\"obj123\":{\"value\":\"Hello world!\"}}";

    context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);
    assertEquals("Message text", "Hello world!", context.getMessageML().asText());
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello world!</div>", context.getPresentationML());
  }

  @Test
  public void testParseFreemarkerAltEntityObject() throws Exception {
    String message = "<messageML>${entity['obj123'].value}</messageML>";
    String data = "{\"obj123\":{\"value\":\"Hello world!\"}}";

    context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);

    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello world!</div>", context.getPresentationML());
  }

  @Test
  public void testParseFreemarkerResolvingAClass() throws Exception {
    String message =
        "<messageML><#assign ex=\"freemarker.template.utility.Execute\"?new()> ${ ex(\"pwd\") }</messageML>";
    String data = "";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Error parsing Freemarker template: invalid input at line 1, column 12");
    context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testParseFreemarkerInvalidContainerObject() throws Exception {
    String message = "<messageML>${obj['obj123'].value}</messageML>";
    String data = "{\"obj123\":{\"value\":\"Hello world!\"}}";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Error parsing Freemarker template: invalid input at line 1, column 14");
    context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testParseFreemarkerEmptyTemplate() {
    String message = "<messageML>${}</messageML>";

    try {
      context.parseMessageML(message, null, MessageML.MESSAGEML_VERSION);
    fail("Should have thrown an exception");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message",
              "Error parsing EntityJSON: Syntax error in template \"messageML\" in line 1, column 14:\n"
              + "Encountered \"}\", but was expecting one of:\n"
              + "    <STRING_LITERAL>\n"
              + "    <RAW_STRING>\n"
              + "    \"false\"\n"
              + "    \"true\"\n"
              + "    <INTEGER>\n"
              + "    <DECIMAL>\n"
              + "    \".\"\n"
              + "    \"+\"\n"
              + "    \"-\"\n"
              + "    \"!\"\n"
              + "    \"[\"\n"
              + "    \"(\"\n"
              + "    \"{\"\n"
              + "    <ID>",
          e.getMessage());
    }
  }

  @Test
  public void testParseMarkdown() throws Exception {
    String message = getPayload("payloads/messageml_v1_payload.json");
    JsonNode messageNode = MAPPER.readTree(message);

    final String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello!"
        + "<table><tr><td>A1</td><td>B1</td></tr><tr><td>A2</td><td>B2</td></tr></table>"
        + "<b>bold</b> <i>italic</i> "
        + "<span class=\"entity\" data-entity-id=\"keyword1\">#hashtag</span> "
        + "<span class=\"entity\" data-entity-id=\"keyword2\">$cashtag</span> "
        + "<span class=\"entity\" data-entity-id=\"mention3\">@Bot User01</span> "
        + "<a href=\"http://example.com\">http://example.com</a>"
        + "<ul>"
        + "<li>list</li>"
        + "<li>item</li>"
        + "</ul><br/>"
        + "<table><tr><td>X1</td><td>Y1</td></tr><tr><td>X2</td><td>Y2</td></tr></table>"
        + "</div>";
    final String expectedMarkdown = "Hello!"
        + "Table:\n"
        + "---\n"
        + "A1 | B1\n"
        + "A2 | B2\n"
        + "---\n"
        + "**bold** _italic_ #hashtag $cashtag @Bot User01 http://example.com\n"
        + "- list\n"
        + "- item\n"
        + "Table:\n"
        + "---\n"
        + "X1 | Y1\n"
        + "X2 | Y2\n"
        + "---\n";
    final JsonNode expectedEntities = MAPPER.readTree("{\n"
        + "    \"hashtags\": [{\n"
        + "        \"id\": \"#hashtag\",\n"
        + "        \"text\": \"#hashtag\",\n"
        + "        \"indexStart\": 55,\n"
        + "        \"indexEnd\": 63,\n"
        + "        \"type\": \"KEYWORD\"\n"
        + "    }, {\n"
        + "        \"id\": \"$cashtag\",\n"
        + "        \"text\": \"$cashtag\",\n"
        + "        \"indexStart\": 64,\n"
        + "        \"indexEnd\": 72,\n"
        + "        \"type\": \"KEYWORD\"\n"
        + "    }],\n"
        + "    \"userMentions\": [{\n"
        + "        \"id\": 123456789,\n"
        + "        \"screenName\": \"bot.user1\",\n"
        + "        \"prettyName\": \"Bot User01\",\n"
        + "        \"text\": \"@Bot User01\",\n"
        + "        \"indexStart\": 73,\n"
        + "        \"indexEnd\": 84,\n"
        + "        \"userType\": \"lc\",\n"
        + "        \"type\": \"USER_FOLLOW\"\n"
        + "    }],\n"
        + "    \"urls\": [{\n"
        + "        \"text\": \"http://example.com\",\n"
        + "        \"id\": \"http://example.com\",\n"
        + "        \"expandedUrl\": \"http://example.com\",\n"
        + "        \"indexStart\": 85,\n"
        + "        \"indexEnd\": 103,\n"
        + "        \"type\": \"URL\"\n"
        + "    }]\n"
        + "}");

    final String generatedEntities = "{"
        + "\"keyword1\":{"
        + "\"type\":\"org.symphonyoss.taxonomy\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.taxonomy.hashtag\","
        + "\"value\":\"hashtag\""
        + "}]},"
        + "\"keyword2\":{"
        + "\"type\":\"org.symphonyoss.fin.security\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.fin.security.id.ticker\","
        + "\"value\":\"cashtag\""
        + "}]},"
        + "\"mention3\":{"
        + "\"type\":\"com.symphony.user.mention\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"com.symphony.user.userId\","
        + "\"value\":\"123456789\""
        + "}]}"
        + "}";
    final JsonNode expectedEntityJson = MAPPER.readTree(generatedEntities);

    context.parseMarkdown(messageNode.get("text").textValue(), messageNode.get("entities"), messageNode.get("media"));

    MessageML messageML = context.getMessageML();
    assertNotNull("MessageML", messageML);
    assertEquals("Chime", false, messageML.isChime());

    List<Element> children = messageML.getChildren();
    assertEquals("MessageML children", 16, children.size());

    assertEquals("Child #1 class", TextNode.class, children.get(0).getClass());
    assertEquals("Child #1 text", "Hello!", ((TextNode) children.get(0)).getText());
    assertTrue("Child #1 attributes", children.get(0).getAttributes().isEmpty());
    assertEquals("Child #1 children", 0, children.get(0).getChildren().size());

    assertEquals("Child #7 class", HashTag.class, children.get(6).getClass());
    assertEquals("Child #7 text", "hashtag", ((HashTag) children.get(6)).getTag());
    assertTrue("Child #7 attributes", children.get(6).getAttributes().isEmpty());
    assertTrue("Child #7 children", children.get(6).getChildren().isEmpty());

    assertEquals("Child #9 class", CashTag.class, children.get(8).getClass());
    assertEquals("Child #9 text", "cashtag", ((CashTag) children.get(8)).getTag());
    assertEquals("Child #9 attributes", 0, children.get(8).getAttributes().size());
    assertEquals("Child #9 children", 0, children.get(8).getChildren().size());

    assertEquals("Child #11 class", Mention.class, children.get(10).getClass());
    assertEquals("Child #11 user ID", 123456789, ((Mention) children.get(10)).getUserPresentation().getId());
    assertEquals("Child #11 user email", "bot.user1@localhost.com",
        ((Mention) children.get(10)).getUserPresentation().getEmail());
    assertEquals("Child #11 user name", "bot.user1",
        ((Mention) children.get(10)).getUserPresentation().getScreenName());
    assertTrue("Child #11 attributes", children.get(10).getAttributes().isEmpty());
    assertTrue("Child #11 children", children.get(10).getChildren().isEmpty());

    assertEquals("Child #13 class", Link.class, children.get(12).getClass());
    assertEquals("Child #13 text", new URI("http://example.com"), ((Link) children.get(12)).getUri());
    assertEquals("Child #13 attributes", 1, children.get(12).getAttributes().size());
    assertEquals("Child #13 attribute", "http://example.com", children.get(12).getAttribute("href"));
    assertEquals("Child #13 children", 0, children.get(12).getChildren().size());

    assertEquals("Child #14 class", BulletList.class, children.get(13).getClass());
    assertEquals("Child #14 attributes", 0, children.get(13).getAttributes().size());
    assertEquals("Child #14 children", 2, children.get(13).getChildren().size());

    validateMessageML(expectedPresentationML, expectedEntityJson, expectedMarkdown, expectedEntities);
  }

  @Test
  public void testParseMarkdownWithHtmlTag() throws Exception {
    String markdown = "<div class=\"foo\">*Markdown*</div> *Markdown* <hr/>";
    JsonNode entities = new ObjectNode(JsonNodeFactory.instance);
    context.parseMarkdown(markdown, entities, null);

    assertEquals("Generated PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "&lt;div class=&quot;foo&quot;&gt;<i>Markdown</i>&lt;/div&gt; <i>Markdown</i> &lt;hr/&gt;</div>",
        context.getPresentationML());
    assertEquals("Generated Markdown", "<div class=\"foo\">_Markdown_</div> _Markdown_ <hr/>",
        context.getMarkdown());
  }

  @Test
  public void testParseMarkdownWithNbsp() throws Exception {
    String message = "Hello\u00A0world!";
    context.parseMarkdown(message, null, null);

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello world!</div>";
    String expectedMarkdown = "Hello world!";

    assertEquals("Presentation ML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
  }

  @Test
  public void testParseMarkdownMissingEntityId() throws Exception {
    String message = "Hello #world";

    JsonNode entities = MAPPER.readTree("{"
        + "        \"hashtags\": [{"
        + "          \"indexStart\": 6,"
        + "          \"indexEnd\": 12,"
        + "          \"type\": \"KEYWORD\""
        + "        }]"
        + "      }");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Required field \"id\" missing from the entity payload");
    context.parseMarkdown(message, entities, null);
  }

  @Test
  public void testParseMarkdownMissingEntityType() throws Exception {
    String message = "Hello #world";
    JsonNode entities = MAPPER.readTree("{"
        + "        \"hashtags\": [{"
        + "          \"indexStart\": 6,"
        + "          \"indexEnd\": 12,"
        + "          \"id\": \"#world\""
        + "        }]"
        + "      }");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Required field \"type\" missing from the entity payload");
    context.parseMarkdown(message, entities, null);
  }

  @Test
  public void testParseMarkdownInvalidEntityIndices() throws Exception {
    String message = "Hello #world";
    JsonNode entities = MAPPER.readTree("{"
        + "        \"hashtags\": [{"
        + "          \"indexStart\": 0,"
        + "          \"indexEnd\": 0,"
        + "          \"id\": \"?AQB6QI3LzHsTHfacv2E9x4QFAAAAAAAAAAAAAAAAAAAAAK5saXEaylHzAK65LoYFTqRcsI+4Qrc=\",\n"
        + "          \"type\": \"KEYWORD\""
        + "        }]"
        + "      }");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid entity payload: "
        + "?AQB6QI3LzHsTHfacv2E9x4QFAAAAAAAAAAAAAAAAAAAAAK5saXEaylHzAK65LoYFTqRcsI+4Qrc= (start index: 0, end index: 0)");
    context.parseMarkdown(message, entities, null);
  }

  @Test
  public void testParseMarkdownStandaloneHash() throws Exception {
    String markdown = "# test";
    context.parseMarkdown(markdown, null, null);

    assertEquals("Text", "# test", context.getText());
    assertEquals("Markdown", "# test", context.getMarkdown());
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"># test</div>", context.getPresentationML());
  }

  @Test
  public void testFailOnInvalidMessageML() throws Exception {
    String invalidMarkup = "<message></message>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Root tag must be <messageML>");
    context.parseMessageML(invalidMarkup, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFailOnBlankMessageML() throws Exception {
    String invalidMarkup = "";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Error parsing message: the message cannot be null or empty");
    context.parseMessageML(invalidMarkup, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFailOnNullMessageML() throws Exception {
    String invalidMarkup = null;
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Error parsing message: the message cannot be null or empty");
    context.parseMessageML(invalidMarkup, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFailOnInvalidCharacters() throws Exception {
    String invalidMarkup = "<messageML>Hello\bworld!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid control characters in message");
    context.parseMessageML(invalidMarkup, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFailOnInvalidAttibuteMarkup() throws Exception {
    String invalidMarkup = "<messageML><div class=invalid>Test</div></messageML>";
    expectedException.expect(InvalidInputException.class);
    // Local test throws the correct message, but Sonar doesn't, so commenting this check out
//    expectedException.expectMessage("Invalid messageML: "
//        + "Open quote is expected for attribute \"class\" associated with an  element type  \"div\"");
    context.parseMessageML(invalidMarkup, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFailOnInvalidTagMarkup() throws Exception {
    String invalidMarkup = "<messageML><div class=\"invalid\">Test</span></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid messageML: "
        + "The element type \"div\" must be terminated by the matching end-tag \"</div>\"");
    context.parseMessageML(invalidMarkup, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFailOnInvalidTag() throws Exception {
    String invalidMarkup = "<messageML><script>Test</script></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid MessageML content at element \"script\"");
    context.parseMessageML(invalidMarkup, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFailOnInvalidJSON() throws Exception {
    String message = "<messageML>MessageML</messageML>";
    String json = "{invalid: json}";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Error parsing EntityJSON: "
        + "Unexpected character ('i' (code 105)): was expecting double-quote to start field name");
    context.parseMessageML(message, json, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFailOnMismatchedEntities() throws Exception {
    String message = "<messageML><div class=\"entity\" data-entity-id=\"obj123\">This will fail</div></messageML>";
    String entityJson = "{\"obj456\": \"Won't match\"}";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Error processing EntityJSON: "
        + "no entity data provided for \"data-entity-id\"=\"obj123\"");
    context.parseMessageML(message, entityJson, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFailOnNoTemplateData() throws Exception {
    String message = getPayload("payloads/single_jira_ticket.messageml");

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Error parsing Freemarker template: invalid input at line 5, column 29");
    context.parseMessageML(message, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testFailOnInvalidTemplate() throws Exception {
    String message = "<messageML><#invalid>template</#invalid>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Syntax error in template \"messageML\" in line 1, column 13");
    context.parseMessageML(message, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testGetMessageMLFailOnUnparsedMessage() throws Exception {
    expectedException.expect(IllegalStateException.class);
    expectedException.expectMessage("The message hasn't been parsed yet. "
        + "Please call MessageMLContext.parse() first.");
    context.getMessageML();
  }

  @Test
  public void testGetPresentationMLFailOnUnparsedMessage() throws Exception {
    expectedException.expect(IllegalStateException.class);
    expectedException.expectMessage("The message hasn't been parsed yet. "
        + "Please call MessageMLContext.parse() first.");
    context.getPresentationML();
  }

  @Test
  public void testGetMarkdownMessageMLFailOnUnparsedMessage() throws Exception {
    expectedException.expect(IllegalStateException.class);
    expectedException.expectMessage("The message hasn't been parsed yet. "
        + "Please call MessageMLContext.parse() first.");
    context.getMarkdown();
  }

  @Test
  public void testGetEntitiesFailOnUnparsedMessage() throws Exception {
    expectedException.expect(IllegalStateException.class);
    expectedException.expectMessage("The message hasn't been parsed yet. "
        + "Please call MessageMLContext.parse() first.");
    context.getEntities();
  }

  @Test
  public void testGetEntityJSONFailOnUnparsedMessage() throws Exception {
    expectedException.expect(IllegalStateException.class);
    expectedException.expectMessage("The message hasn't been parsed yet. "
        + "Please call MessageMLContext.parse() first.");
    context.getEntityJson();
  }

  @Test
  public void testGetText() throws Exception {
    String message = getPayload("payloads/templated_message_all_tags.messageml");
    String data = getPayload("payloads/templated_message_all_tags.json");

    String expectedText =
        "   Sample JIRA issue  Bot User01 updated Bug  SAM-24,Sample Bug Blocker     FieldOld Value => New Value     "
            + "resolution Open => Done   status To Do => Done         Field Value     Assignee @Bot User01   Labels   "
            + "#production #major       Priority Highest   Status Done     ";

    context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);
    String text = context.getText();
    assertEquals("Message as text", expectedText, text);
  }

  @Test
  public void testGetTestWhitespaceLogic() throws Exception {
    String message = "<messageML><span>\nfoo</span>bar<span>\n</span> baz<span>qux\n</span></messageML>";
    context.parseMessageML(message, null, MessageML.MESSAGEML_VERSION);

    assertEquals("Message as text", " foobar  bazqux ", context.getText());
    assertEquals("Message as text, preserve whitespace", " foo bar    baz qux ", context.getText(true));
    assertEquals("Message as text, trim whitespace", "foo bar baz qux", context.getText(false));
  }

  @Test
  public void testEscapeReservedCharsFromMessageML() throws Exception {
    String messageML = "½ ¼ ¾ [ ] \\ ; ' , . / ~ ! @ # $ % - = ^ &amp; * ( ) _ + { } | : \" &lt; &gt; ? "
        + "<i>italic</i> <b>bold</b> <hash tag=\"hashtag\"/>";
    String excpectedPresentationML = "½ ¼ ¾ [ ] \\ ; ' , . / ~ ! @ # $ % - = ^ &amp; * ( ) _ + { } | : &quot; &lt; &gt; ? "
        + "<i>italic</i> <b>bold</b> <span class=\"entity\" data-entity-id=\"keyword1\">#hashtag</span>";
    String expectedMarkdown = "½ ¼ ¾ [ ] \\ ; ' , . / ~ ! @ # $ % \\- = ^ & \\* ( ) \\_ \\+ { } | : \" < > ? "
        + "_italic_ **bold** #hashtag";
    JsonNode expectedEntities = MAPPER.readTree("{\"hashtags\": [\n"
        + "    {\n"
        + "      \"id\": \"#hashtag\",\n"
        + "      \"text\": \"#hashtag\",\n"
        + "      \"indexStart\": 90,\n"
        + "      \"indexEnd\": 98,\n"
        + "      \"type\": \"KEYWORD\"\n"
        + "    }\n"
        + "  ]\n"
        + "}");

    context.parseMessageML(String.format("<messageML>%s</messageML>", messageML), null, MessageML.MESSAGEML_VERSION);

    assertEquals("Generated PresentationML",
        String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">%s</div>", excpectedPresentationML),
        context.getPresentationML());
    assertEquals("Generated Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Generated entities", expectedEntities, context.getEntities());
  }

  @Test
  public void testEscapeReservedCharsFromMarkdown() throws Exception {
    String markdown = "½ ¼ ¾ [ ] \\ ; ' , . / ~ ! @ # $ % - = ^ & * ( ) _ + { } | : \" < > ? "
        + "_italic_ **bold** #hashtag";
    JsonNode entities = MAPPER.readTree("{\"hashtags\": [\n"
        + "    {\n"
        + "      \"id\": \"#hashtag\",\n"
        + "      \"text\": \"#hashtag\",\n"
        + "      \"indexStart\": 86,\n"
        + "      \"indexEnd\": 94,\n"
        + "      \"type\": \"KEYWORD\"\n"
        + "    }\n"
        + "  ]\n"
        + "}");
    String excpectedPresentationML = "½ ¼ ¾ [ ] \\ ; ' , . / ~ ! @ # $ % - = ^ &amp; * ( ) _ + { } | : &quot; &lt; &gt; ? "
        + "<i>italic</i> <b>bold</b> <span class=\"entity\" data-entity-id=\"keyword1\">#hashtag</span>";
    String expectedMarkdown = "½ ¼ ¾ [ ] \\ ; ' , . / ~ ! @ # $ % \\- = ^ & \\* ( ) \\_ \\+ { } | : \" < > ? "
        + "_italic_ **bold** #hashtag";
    JsonNode expectedEntities = MAPPER.readTree("{\"hashtags\": [\n"
        + "    {\n"
        + "      \"id\": \"#hashtag\",\n"
        + "      \"text\": \"#hashtag\",\n"
        + "      \"indexStart\": 90,\n"
        + "      \"indexEnd\": 98,\n"
        + "      \"type\": \"KEYWORD\"\n"
        + "    }\n"
        + "  ]\n"
        + "}");


    context.parseMarkdown(markdown, entities, null);

    assertEquals("Generated PresentationML",
        String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">%s</div>", excpectedPresentationML),
        context.getPresentationML());
    assertEquals("Generated Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Generated entities", expectedEntities, context.getEntities());
  }

  @Test
  public void testEscapeReservedCharsFromEscapedMarkdown() throws Exception {
    String markdown = "½ ¼ ¾ [ ] \\ ; ' , . / ~ ! @ # $ % \\- = ^ & \\* ( ) \\_ \\+ { } | : \" < > ? "
        + "_italic_ **bold** #hashtag";
    JsonNode entities = MAPPER.readTree("{\"hashtags\": [\n"
        + "    {\n"
        + "      \"id\": \"#hashtag\",\n"
        + "      \"text\": \"#hashtag\",\n"
        + "      \"indexStart\": 90,\n"
        + "      \"indexEnd\": 98,\n"
        + "      \"type\": \"KEYWORD\"\n"
        + "    }\n"
        + "  ]\n"
        + "}");
    String excpectedPresentationML = "½ ¼ ¾ [ ] \\ ; ' , . / ~ ! @ # $ % - = ^ &amp; * ( ) _ + { } | : &quot; &lt; &gt; ? "
        + "<i>italic</i> <b>bold</b> <span class=\"entity\" data-entity-id=\"keyword1\">#hashtag</span>";
    String expectedMarkdown = "½ ¼ ¾ [ ] \\ ; ' , . / ~ ! @ # $ % \\- = ^ & \\* ( ) \\_ \\+ { } | : \" < > ? "
        + "_italic_ **bold** #hashtag";
    JsonNode expectedEntities = MAPPER.readTree("{\"hashtags\": [\n"
        + "    {\n"
        + "      \"id\": \"#hashtag\",\n"
        + "      \"text\": \"#hashtag\",\n"
        + "      \"indexStart\": 90,\n"
        + "      \"indexEnd\": 98,\n"
        + "      \"type\": \"KEYWORD\"\n"
        + "    }\n"
        + "  ]\n"
        + "}");


    context.parseMarkdown(markdown, entities, null);

    assertEquals("Generated PresentationML",
        String.format("<div data-format=\"PresentationML\" data-version=\"2.0\">%s</div>", excpectedPresentationML),
        context.getPresentationML());
    assertEquals("Generated Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("Generated entities", expectedEntities, context.getEntities());
  }

  @Test
  public void testFindElements() throws Exception {
    String message = getPayload("payloads/templated_message_all_tags.messageml");
    String data = getPayload("payloads/templated_message_all_tags.json");

    context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);

    MessageML messageML = context.getMessageML();
    List<Element> mentions = messageML.findElements(Mention.class);
    List<Element> hashtags = messageML.findElements(HashTag.MESSAGEML_TAG);
    List<Element> labels = messageML.findElements("class", "label");

    assertEquals("Mention count", 1, mentions.size());
    assertEquals("Hashtag count", 2, hashtags.size());
    assertEquals("Label count", 3, labels.size());

  }

  private String getPayload(String filename) throws IOException {
    ClassLoader classLoader = getClass().getClassLoader();
    try(Scanner scanner = new Scanner(classLoader.getResourceAsStream(filename)))
    {
      return scanner.useDelimiter("\\A").next();
    }
  }

}