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

package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.TestDataProvider;

import java.util.Collections;

/**
 * Base class for unit tests of MessageML elements. Sets up fields used across all other tests and verifies general cases.
 *
 * Each element should have at least the following tests:
 * <ul>
 *   <li>parsing of its MessageML representation</li>
 *   <li>parsing of its PresentationML representation</li>
 *   <li>parsing of its Markdown representation</li>
 *   <li>expected exception on invalid attributes</li>
 *   <li>expected exception on invalid child elements</li>
 * </ul>
 */
public class ElementTest {
  static final ObjectMapper MAPPER = new ObjectMapper();
  final IDataProvider dataProvider = new TestDataProvider();

  protected static final String ACTION_BTN_ELE = "<button type=\"action\" name=\"actionName\">Send</button>";
  protected static final String ACTION_BTN_MD = "(Button:Send)";

  @Rule
  public final ExpectedException expectedException = ExpectedException.none();

  protected MessageMLContext context;

  @Before
  public void setUp() {
    context = new MessageMLContext(dataProvider);
  }

  @Test
  public void testMessageML() throws Exception {
    for (String noContent : new String[] {"<messageML></messageML>", "<messageML/>"}) {
      context.parseMessageML(noContent, null, MessageML.MESSAGEML_VERSION);

      Element messageML = context.getMessageML();
      assertEquals("Element class", MessageML.class, messageML.getClass());
      assertEquals("Element tag name", "messageML", messageML.getMessageMLTag());
      assertEquals("Element attributes", Collections.emptyMap(), messageML.getAttributes());
      assertEquals("Element children", Collections.<Element>emptyList(), messageML.getChildren());
      assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\"></div>",
          context.getPresentationML());
      assertEquals("Markdown", "", context.getMarkdown());
      assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
    }

    String withContent = "<messageML>Hello world!</messageML>";
    context.parseMessageML(withContent, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());
    assertEquals("Child element", "Hello world!", messageML.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello world!</div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello world!", context.getMarkdown());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testMessageMLInvalidAttr() throws Exception {
    String invalidAttr = "<messageML class=\"label\"></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"messageML\" may not have attributes");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testMessageMLInvalidElementType() throws Exception {
    String invalidElement = "<messageML><?processing-instruction content?></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid element \"processing-instruction\"");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testMessageMLNoVersion() throws Exception {
    String input = "<messageML>Hello world!</messageML>";

    context.parseMessageML(input, null, null);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());
    assertEquals("Child element", "Hello world!", messageML.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello world!</div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello world!", context.getMarkdown());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testPresentationML() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello world!</div>";

    context.parseMessageML(input, null, null);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());
    assertEquals("Child element", "Hello world!", messageML.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello world!</div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello world!", context.getMarkdown());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testPresentationMLMissingAttributes() throws Exception {
    String input = "<div>Hello world!</div>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Malformed PresentationML. "
        + "The attributes \"data-format\" and \"data-version\" are required.");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLInvalidFormatAttribute() throws Exception {
    String input = "<div data-format=\"invalid\" data-version=\"2.0\">Hello world!</div>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Malformed PresentationML. "
        + "The attributes \"data-format\" and \"data-version\" are required.");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testEntityJsonInvalidEntityNode() throws Exception {
    String input = "<messageML>Hello <div class=\"entity\" data-entity-id=\"obj123\">entity</div>!</messageML>";
    String entityJson = "{\"obj123\":\"Invalid\"}";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Error processing EntityJSON: the node \"obj123\" has to be an object");
    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testEntityJsonMissingEntityNode() throws Exception {
    String input = "<messageML>Hello <div class=\"entity\" data-entity-id=\"obj123\">entity</div>!</messageML>";
    String entityJson = "{\"invalid\":{\"key\":\"value\"}}";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Error processing EntityJSON: no entity data provided for "
        + "\"data-entity-id\"=\"obj123\"");
    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLEntity() throws Exception {
    String input = "<messageML>Hello <span class=\"entity\" data-entity-id=\"obj123\">world</span>!</messageML>";

    String entityJson = "{\"obj123\":{"
        + "\"type\":\"com.example.custom\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"com.example.custom.entity\","
        + "\"value\":\"Hello world!\""
        + "}]}}";

    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();

    assertEquals("Element children", 3, messageML.getChildren().size());

    Element entity = messageML.getChildren().get(1);

    assertEquals("Element class", Span.class, entity.getClass());
    assertEquals("Element tag name", "span", entity.getMessageMLTag());
    assertEquals("Element attributes", 2, entity.getAttributes().size());
    assertEquals("Element class attribute", "entity", entity.getAttribute("class"));
    assertEquals("Element data id", "obj123", entity.getAttribute("data-entity-id"));
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello <span class=\"entity\" "
            + "data-entity-id=\"obj123\">world</span>!</div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello world!", context.getMarkdown());
    assertEquals("EntityJSON", entityJson, MAPPER.writeValueAsString(context.getEntityJson()));
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testPlainText() throws Exception {
    String invalidInput = "Hello world!";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid messageML: Content is not allowed in prolog.");
    context.parseMessageML(invalidInput, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testNestedEntity() throws Exception {
    String message = "<messageML>"
        + "<div class=\"entity\" data-entity-id=\"obj123\">"
        + "<cash tag=\"ibm\"/>"
        + "</div>"
        + "</messageML>";
    String data = "{\"obj123\": {"
        + "\"type\": \"com.acme.entity\","
        + "\"version\": \"1.0\","
        + "\"id\": ["
        + "{\"type\": \"com.acme.entity.foo\","
        + "\"value\": \"bar\"}]}}";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<div class=\"entity\" data-entity-id=\"obj123\">"
        + "<span class=\"entity\" data-entity-id=\"keyword1\">$ibm</span>"
        + "</div>"
        + "</div>";

    String expectedMarkdown = "$ibm\n\n";

    context.parseMessageML(message, data, MessageML.MESSAGEML_VERSION);
    String presentationML = context.getPresentationML();
    JsonNode entityJson = context.getEntityJson();
    String markdown = context.getMarkdown();
    JsonNode entities = context.getEntities();

    assertEquals("PresentationML", expectedPresentationML, presentationML);
    assertEquals("Markdown", expectedMarkdown, markdown);

    assertEquals("EntityJson size", 1, entityJson.size());
    assertNotNull("Entity", entityJson.get("obj123"));
    JsonNode entity = entityJson.get("obj123");
    assertEquals("Entity size", 4, entity.size());
    assertEquals("Entity type", "com.acme.entity", entity.get("type").textValue());
    assertEquals("Entity version", "1.0", entity.get("version").textValue());
    assertEquals("Entity id", "com.acme.entity.foo", entity.get("id").get(0).get("type").textValue());
    assertEquals("Entity value", "bar", entity.get("id").get(0).get("value").textValue());
    assertNotNull("Keyword entity", entity.get("keyword1"));
    JsonNode keyword = entity.get("keyword1");
    assertEquals("Entity size", 3, keyword.size());
    assertEquals("Entity type", "org.symphonyoss.fin.security", keyword.get("type").textValue());
    assertEquals("Entity version", "1.0", keyword.get("version").textValue());
    assertEquals("Entity id", "org.symphonyoss.fin.security.id.ticker",
        keyword.get("id").get(0).get("type").textValue());
    assertEquals("Entity value", "ibm", keyword.get("id").get(0).get("value").textValue());

    assertEquals("Legacy entities size", 1, entities.size());
    assertNotNull("Legacy hashtags", entities.get("hashtags"));
    JsonNode legacyHashtags = entities.get("hashtags");
    assertEquals("Legacy hashtags count", 1, legacyHashtags.size());
    assertEquals("Legacy hashtag id", "$ibm", legacyHashtags.get(0).get("id").textValue());
    assertEquals("Legacy hashtag text", "$ibm", legacyHashtags.get(0).get("text").textValue());
    assertEquals("Legacy hashtag type", "KEYWORD", legacyHashtags.get(0).get("type").textValue());
    assertEquals("Legacy hashtag type", 0, legacyHashtags.get(0).get("indexStart").intValue());
    assertEquals("Legacy hashtag type", 4, legacyHashtags.get(0).get("indexEnd").intValue());
  }

  @Test
  public void testBodyInvalidParent() {
    String input = "<messageML><body>Hello world!</body></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Element \"body\" can only be a child of the following elements: [card]", e.getMessage());
    }
  }

}
