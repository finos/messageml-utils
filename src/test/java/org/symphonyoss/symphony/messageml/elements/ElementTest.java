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
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import com.fasterxml.jackson.core.JsonProcessingException;
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
import org.symphonyoss.symphony.messageml.util.UserPresentation;

import java.util.Collections;
import java.util.HashMap;

public class ElementTest {
  private static final ObjectMapper MAPPER = new ObjectMapper();
  private final IDataProvider dataProvider = new TestDataProvider();

  @Rule
  public final ExpectedException expectedException = ExpectedException.none();

  private MessageMLContext context;

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
  public void testPresentationMLShorthandChime() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\"><chime/></div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"chime\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLShorthandHashTag() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\"><hash tag=\"invalid\"/></div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"hash\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLShorthandCashTag() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\"><cash tag=\"invalid\"/></div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"cash\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLShorthandMention() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\"><mention uid=\"1\"/></div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"mention\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLShorthandCard() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\">"
        + "<card iconSrc=\"icon.png\" class=\"barStyle\">invalid</card>"
        + "</div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"card\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLShorthandCardHeader() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\">"
        + "<div class=\"card\">"
        + "<header>invalid</header>"
        + "</div>"
        + "</div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"header\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLShorthandCardBody() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\">"
        + "<div class=\"card\">"
        + "<div class=\"cardHeader\">"
        + "<body>invalid</body>"
        + "</div>"
        + "</div>"
        + "</div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"body\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
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
  public void testPlainText() throws Exception {
    String invalidInput = "Hello world!";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid messageML: Content is not allowed in prolog.");
    context.parseMessageML(invalidInput, null, MessageML.MESSAGEML_VERSION);
  }

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
    String invalidAttr = "<messageML><br style=\"label\"/></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"br\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testLineBreakInvalidContent() throws Exception {
    String invalidContent = "<messageML><br>Hello world!</br></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"br\" may not have child elements or text content");
    context.parseMessageML(invalidContent, null, MessageML.MESSAGEML_VERSION);
  }

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
    String invalidAttr = "<messageML><hr style=\"label\"/></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"hr\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testHorizontalRuleInvalidContent() throws Exception {
    String invalidContent = "<messageML><hr>Hello world!</hr></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"hr\" may not have child elements or text content");
    context.parseMessageML(invalidContent, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testBold() throws Exception {
    String input = "<messageML><b>Hello world!</b></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element bold = messageML.getChildren().get(0);

    assertEquals("Element class", Bold.class, bold.getClass());
    assertEquals("Element tag name", "b", bold.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), bold.getAttributes());
    assertEquals("Element children", 1, bold.getChildren().size());
    assertEquals("Child element", TextNode.class, bold.getChildren().get(0).getClass());
    assertEquals("Child element text", "Hello world!", bold.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\"><b>Hello world!</b></div>",
        context.getPresentationML());
    assertEquals("Markdown", "**Hello world!**", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><b class=\"label\">Hello world!</b></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    bold = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, bold.getAttributes().size());
    assertEquals("Attribute", "label", bold.getAttribute("class"));
  }

  @Test
  public void testBoldInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><b style=\"label\">Hello world!</b></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"b\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

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
  }

  @Test
  public void testItalicInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><i style=\"label\">Hello world!</i></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"i\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPreformatted() throws Exception {
    String input = "<messageML>\n<pre>\n\t<span>\n\t\tHello\n\t</span>\n\tworld!\n</pre>\n</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 3, messageML.getChildren().size());

    Element pre = messageML.getChildren().get(1);

    assertEquals("Element class", Preformatted.class, pre.getClass());
    assertEquals("Element tag name", "pre", pre.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), pre.getAttributes());
    assertEquals("Element children", 3, pre.getChildren().size());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + " <pre>\n\t<span>\n\t\tHello\n\t</span>\n\tworld!\n</pre> </div>", context.getPresentationML());
    assertEquals("Markdown", " \n\n\t\n\t\tHello\n\t\n\tworld!\n\n ", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><pre class=\"label\">Hello world!</pre></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    pre = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, pre.getAttributes().size());
    assertEquals("Attribute", "label", pre.getAttribute("class"));
  }

  @Test
  public void testPreformattedInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><pre style=\"invalid\">Hello world!</pre></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"pre\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPreformattedInvalidContent() throws Exception {
    String invalidContent = "<messageML><pre><p>Hello world!</p></pre></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"p\" is not allowed in \"pre\"");
    context.parseMessageML(invalidContent, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPreformattedInvalidMarkup() throws Exception {
    String invalidMarkup = "<messageML><pre><span>Hello world!</pre></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The element type \"span\" must be terminated by the matching end-tag \"</span>\"");
    context.parseMessageML(invalidMarkup, null, MessageML.MESSAGEML_VERSION);
  }

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
          "<div data-format=\"PresentationML\" data-version=\"2.0\"><h" + level + ">Hello world!</h" + level + "></div>",
          context.getPresentationML());
      assertEquals("Markdown", "**Hello world!**", context.getMarkdown());
      assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
      assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

      String withAttr = "<messageML><h" + level + " class=\"label\">Hello world!</h" + level + "></messageML>";
      context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
      header = context.getMessageML().getChildren().get(0);
      assertEquals("Attribute count", 1, header.getAttributes().size());
      assertEquals("Attribute", "label", header.getAttribute("class"));
    }
  }

  @Test
  public void testHeadersInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><h1 style=\"label\">Hello world!</h1></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"h1\"");
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
  }

  @Test
  public void testParagraphInvalidAttr() throws Exception {
    String invalidAttr = "<messageML>Hello<p style=\"label\">world</p>!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"p\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSpan() throws Exception {
    String input = "<messageML>Hello <span>world</span>!</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 3, messageML.getChildren().size());

    Element span = messageML.getChildren().get(1);

    assertEquals("Element class", Span.class, span.getClass());
    assertEquals("Element tag name", "span", span.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), span.getAttributes());
    assertEquals("Element children", 1, span.getChildren().size());
    assertEquals("Child element", TextNode.class, span.getChildren().get(0).getClass());
    assertEquals("Child element text", "world", span.getChildren().get(0).asText());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello <span>world</span>!</div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello world!", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testSpanInvalidAttr() throws Exception {
    String invalidAttr = "<messageML>Hello<span style=\"label\">world</span>!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"span\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testSpanInvalidEntityId() throws Exception {
    String invalidAttr = "<messageML>Hello<span class=\"label\" data-entity-id=\"id\">world</span>!</messageML>";
    String entityJson = "{\"id\":{\"key\":\"value\"}}";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"data-entity-id\" is only allowed if the element class is "
        + "\"entity\".");
    context.parseMessageML(invalidAttr, entityJson, MessageML.MESSAGEML_VERSION);
  }

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
    String invalidAttr = "<messageML>Hello<div style=\"label\">world</div>!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"div\"");
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

  private void verifyChime(Element messageML) throws Exception {
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element chime = messageML.getChildren().get(0);

    assertEquals("Element class", Chime.class, chime.getClass());
    assertEquals("Element tag name", "chime", chime.getMessageMLTag());
    assertTrue("Is chime", ((MessageML) messageML).isChime());
    assertEquals("Element attributes", Collections.emptyMap(), chime.getAttributes());
    assertEquals("Element children", Collections.<Element>emptyList(), chime.getChildren());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "<audio src=\"https://asset.symphony.com/symphony/audio/chime.mp3\" autoplay=\"true\"/></div>",
        context.getPresentationML());
    assertEquals("Markdown", "", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testChime() throws Exception {
    String input = "<messageML><chime/></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    verifyChime(messageML);
  }

  @Test
  public void testChimeByPresentationML() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<audio src=\"https://asset.symphony.com/symphony/audio/chime.mp3\" autoplay=\"true\"/>"
        + "</div>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    verifyChime(messageML);
  }

  @Test
  public void testChimeInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><chime class=\"label\"/></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"chime\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testChimeInvalidContent() throws Exception {
    String invalidContent = "<messageML><chime>Hello world!</chime></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"chime\" may not have child elements or text content");
    context.parseMessageML(invalidContent, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testChimeInvalidMessage() throws Exception {
    String invalidAttr = "<messageML>Hello <chime/> world!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Chime messages may not have any other content");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testChimeInvalidSrc() throws Exception {
    String invalidAttr = "<messageML>"
        + "<audio src=\"https://invalid.com\" autoplay=\"true\"/>"
        + "</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"src\" value needs to be "
        + "\"https://asset.symphony.com/symphony/audio/chime.mp3\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testImage() throws Exception {
    String input = "<messageML>Hello <img src=\"hello.png\"/> world!</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 3, messageML.getChildren().size());

    Element img = messageML.getChildren().get(1);

    assertEquals("Element class", Image.class, img.getClass());
    assertEquals("Element tag name", "img", img.getMessageMLTag());
    assertEquals("Element attributes", 1, img.getAttributes().size());
    assertEquals("Element attribute value", "hello.png", img.getAttribute("src"));
    assertEquals("Element children", Collections.<Element>emptyList(), img.getChildren());
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\">Hello <img src=\"hello.png\"/> world!</div>",
        context.getPresentationML());
    assertEquals("Markdown", "Hello  world!", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML>Hello <img src=\"hello.png\" class=\"label\"/> world!</messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    img = context.getMessageML().getChildren().get(1);
    assertEquals("Attribute count", 2, img.getAttributes().size());
    assertEquals("Attribute", "label", img.getAttribute("class"));
  }

  @Test
  public void testImageNoAttr() throws Exception {
    String noAttr = "<messageML>Hello <img/> world!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"src\" is required");
    context.parseMessageML(noAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testImageInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><img style=\"label\" src=\"hello.png\"/></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"img\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testImageInvalidContent() throws Exception {
    String invalidContent = "<messageML><img src=\"hello.png\">Hello world!</img></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"img\" may not have child elements or text content");
    context.parseMessageML(invalidContent, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testLink() throws Exception {
    String input = "<messageML><a href=\"https://hello.org\">Hello world!</a></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element link = messageML.getChildren().get(0);

    assertEquals("Element class", Link.class, link.getClass());
    assertEquals("Element tag name", "a", link.getMessageMLTag());
    assertEquals("Element attributes", 1, link.getAttributes().size());
    assertEquals("Element attribute value", "https://hello.org", link.getAttribute("href"));
    assertEquals("Element children", 1, link.getChildren().size());
    assertEquals("Child element", TextNode.class, link.getChildren().get(0).getClass());
    assertEquals("Child element text", "Hello world!", link.getChildren().get(0).asText());
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><a href=\"https://hello.org\">Hello world!</a></div>",
        context.getPresentationML());
    assertEquals("Markdown", "https://hello.org", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", 1, context.getEntities().size());

    JsonNode entity = context.getEntities().get("urls");
    assertNotNull("Entity node", entity);
    assertEquals("Entity count", 1, entity.size());

    assertEquals("Entity text", "https://hello.org", entity.get(0).get("text").textValue());
    assertEquals("Entity id", "https://hello.org", entity.get(0).get("id").textValue());
    assertEquals("Entity expanded URL", "https://hello.org", entity.get(0).get("expandedUrl").textValue());
    assertEquals("Entity start index", 0, entity.get(0).get("indexStart").intValue());
    assertEquals("Entity end index", 17, entity.get(0).get("indexEnd").intValue());
    assertEquals("Entity type", "URL", entity.get(0).get("type").textValue());

    String noText = "<messageML><a href=\"https://hello.org\"/></messageML>";
    context.parseMessageML(noText, null, MessageML.MESSAGEML_VERSION);
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><a href=\"https://hello.org\">https://hello.org</a></div>",
        context.getPresentationML());
    assertEquals("Markdown", "https://hello.org", context.getMarkdown());
  }

  @Test
  public void testLinkNoAttr() throws Exception {
    String noAttr = "<messageML><a>Hello world!</a></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"href\" is required");
    context.parseMessageML(noAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testLinkInvalidAttr() throws Exception {
    String invalidAttr = "<messageML>Hello<a style=\"label\">world</a>!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"a\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testLinkUnsupportedProtocol() throws Exception {
    String invalidUri = "<messageML><a href=\"invalid://hello.org\">Hello world!</a></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("URI scheme \"invalid\" is not supported by the pod.");
    context.parseMessageML(invalidUri, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testLinkInvalidUri() throws Exception {
    String invalidUri = "<messageML><a href=\"[invalid]\">Hello world!</a></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid input: null must be a URI value not \"[invalid]\"");
    context.parseMessageML(invalidUri, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testLinkEmptyUri() throws Exception {
    String invalidUri = "<messageML><a href=\"\">Hello world!</a></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"href\" cannot be empty");
    context.parseMessageML(invalidUri, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testLinkNoProtocol() throws Exception {
    String invalidUri = "<messageML><a href=\"example.com\">Hello world!</a></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"href\" must contain an absolute URI");
    context.parseMessageML(invalidUri, null, MessageML.MESSAGEML_VERSION);
  }

  private void verifyHashTag(Element messageML, String expectedPresentationML, String expectedJson, String expectedText,
      String expectedMarkdown) throws Exception {
    assertEquals("Element children", 3, messageML.getChildren().size());

    Element hashtag = messageML.getChildren().get(1);

    assertEquals("Element class", HashTag.class, hashtag.getClass());
    assertEquals("Element tag name", "hash", hashtag.getMessageMLTag());
    assertEquals("Element text", expectedText, ((HashTag) hashtag).getTag());
    assertEquals("PresentationML", expectedPresentationML, context.getPresentationML());
    assertEquals("Markdown", expectedMarkdown, context.getMarkdown());
    assertEquals("EntityJSON", expectedJson, MAPPER.writeValueAsString(context.getEntityJson()));
    assertEquals("Legacy entities", 1, context.getEntities().size());

    JsonNode entity = context.getEntities().get("hashtags");
    assertNotNull("Entity node", entity);
    assertEquals("Entity count", 1, entity.size());

    assertEquals("Entity text", "#" + expectedText, entity.get(0).get("text").textValue());
    assertEquals("Entity id", "#" + expectedText, entity.get(0).get("id").textValue());
    assertEquals("Entity type", "KEYWORD", entity.get(0).get("type").textValue());
  }

  @Test
  public void testHashTag() throws Exception {
    String input = "<messageML>Hello <hash tag=\"world\"/>!</messageML>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"keyword1\">#world</span>!"
        + "</div>";
    String expectedJson = "{\"keyword1\":{"
        + "\"type\":\"org.symphonyoss.taxonomy\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.taxonomy.hashtag\","
        + "\"value\":\"world\""
        + "}]}}";
    String expectedText = "world";
    String expectedMarkdown = "Hello #world!";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element attributes", Collections.emptyMap(), messageML.getChildren().get(1).getAttributes());
    verifyHashTag(messageML, expectedPresentationML, expectedJson, expectedText, expectedMarkdown);
  }

  @Test
  public void testHashTagNonAlnum() throws Exception {
    String input = "<messageML>Hello <hash tag=\"_hello.w-o-r-l-d_\"/>!</messageML>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"keyword1\">#_hello.w-o-r-l-d_</span>!"
        + "</div>";
    String expectedJson = "{\"keyword1\":{"
        + "\"type\":\"org.symphonyoss.taxonomy\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.taxonomy.hashtag\","
        + "\"value\":\"_hello.w-o-r-l-d_\""
        + "}]}}";
    String expectedText = "_hello.w-o-r-l-d_";
    String expectedMarkdown = "Hello #_hello.w-o-r-l-d_!";

    // Verify by MessageML
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    Element messageML = context.getMessageML();
    assertEquals("Element attributes", Collections.emptyMap(), messageML.getChildren().get(1).getAttributes());
    verifyHashTag(messageML, expectedPresentationML, expectedJson, expectedText, expectedMarkdown);

    // Verify by PresentationML
    context.parseMessageML(expectedPresentationML, expectedJson, MessageML.MESSAGEML_VERSION);
    messageML = context.getMessageML();
    verifyHashTag(messageML, expectedPresentationML, expectedJson, expectedText, expectedMarkdown);
  }

  @Test
  public void testHashTagByPresentationMLDiv() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <div class=\"entity\" data-entity-id=\"hash123\">world</div>!"
        + "</div>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <div class=\"entity\" data-entity-id=\"hash123\">#world</div>!"
        + "</div>";
    String entityJson = "{\"hash123\":{"
        + "\"type\":\"org.symphonyoss.taxonomy\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.taxonomy.hashtag\","
        + "\"value\":\"world\""
        + "}]}}";
    String expectedText = "world";
    String expectedMarkdown = "Hello #world!";

    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element attributes", 1, messageML.getChildren().get(1).getAttributes().size());
    assertEquals("Element class attribute", "entity", messageML.getChildren().get(1).getAttribute("class"));
    verifyHashTag(messageML, expectedPresentationML, entityJson, expectedText, expectedMarkdown);
  }

  @Test
  public void testHashTagByPresentationMLSpan() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"hash123\">world</span>!"
        + "</div>";

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <span class=\"entity\" data-entity-id=\"hash123\">#world</span>!"
        + "</div>";
    String entityJson = "{\"hash123\":{"
        + "\"type\":\"org.symphonyoss.taxonomy\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.taxonomy.hashtag\","
        + "\"value\":\"world\""
        + "}]}}";
    String expectedText = "world";
    String expectedMarkdown = "Hello #world!";

    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element attributes", 1, messageML.getChildren().get(1).getAttributes().size());
    assertEquals("Element class attribute", "entity", messageML.getChildren().get(1).getAttribute("class"));
    verifyHashTag(messageML, expectedPresentationML, entityJson, expectedText, expectedMarkdown);
  }

  @Test
  public void testHashTagByPresentationMLMissingEntityId() throws Exception {
    String input = "<messageML>Hello <span class=\"entity\">world</span>!</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"data-entity-id\" is required");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testHashTagInvalidCharacter() throws Exception {
    String input = "<messageML>Hello <hash tag=\"invalid chars!\"/></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Keywords may only contain alphanumeric characters, underscore, dot and dash");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testHashTagByPresentationMLInvalidCharacter() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "Hello <div class=\"entity\" data-entity-id=\"hash123\">world</div>!"
        + "</div>";

    String entityJson = "{\"hash123\":{"
        + "\"type\":\"org.symphonyoss.taxonomy\","
        + "\"version\":\"1.0\","
        + "\"id\":[{"
        + "\"type\":\"org.symphonyoss.taxonomy.hashtag\","
        + "\"value\":\"invalid chars!\""
        + "}]}}";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Keywords may only contain alphanumeric characters");
    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testHashTagInvalidAttr() throws Exception {
    String invalidAttr = "<messageML>Hello <hash tag=\"world\" class=\"label\"/>!</messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"hash\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
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
    expectedException.expectMessage("Keywords may only contain alphanumeric characters, underscore, dot and dash");
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
    expectedException.expectMessage("Keywords may only contain alphanumeric characters");
    context.parseMessageML(input, entityJson, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCashTagInvalidAttr() throws Exception {
    String invalidAttr = "<messageML>Hello <cash tag=\"world\" class=\"label\"/>!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"cash\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  private void verifyMention(Element messageML, UserPresentation user, String expectedPresentationML, String expectedJson)
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
            + "Hello invalid@email.com!</div>",
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
    JsonNode expectedEntityJSON = MAPPER.readTree("{\"mention1\":{\"type\":\"com.symphony.user.mention\",\"version\":\"1.0\","
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
    JsonNode expectedEntityJSON = MAPPER.readTree("{\"mention1\":{\"type\":\"com.symphony.user.mention\",\"version\":\"1.0\","
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
  public void testBulletList() throws Exception {
    String input = "<messageML><ul><li>hello</li><li>world</li></ul></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element list = messageML.getChildren().get(0);

    assertEquals("Element class", BulletList.class, list.getClass());
    assertEquals("Element tag name", "ul", list.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), list.getAttributes());
    assertEquals("Element children", 2, list.getChildren().size());
    assertEquals("Child element", ListItem.class, list.getChildren().get(0).getClass());
    assertEquals("Child element text", "hello", list.getChildren().get(0).asText());
    assertEquals("Child element", ListItem.class, list.getChildren().get(1).getClass());
    assertEquals("Child element text", "world", list.getChildren().get(1).asText());
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><ul><li>hello</li><li>world</li></ul></div>",
        context.getPresentationML());
    assertEquals("Markdown", "- hello\n- world\n", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML>"
        + "<ul class=\"label\"><li class=\"label\">hello</li><li>world</li></ul>"
        + "</messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    list = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, list.getAttributes().size());
    assertEquals("Attribute", "label", list.getAttribute("class"));
    Element item = list.getChildren().get(0);
    assertEquals("Attribute count", 1, item.getAttributes().size());
    assertEquals("Attribute", "label", item.getAttribute("class"));
  }

  @Test
  public void testBulletListInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><ul style=\"label\"><li>hello</li><li>world</li></ul></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"ul\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testBulletListItemInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><ul><li  style=\"label\">hello</li><li>world</li></ul></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"li\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testBulletListInvalidChildren() throws Exception {
    String invalidChild = "<messageML><ul><div>Hello world!</div></ul></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"div\" is not allowed in \"ul\"");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testBulletListTextContent() throws Exception {
    String invalidChild = "<messageML><ul>Hello world!</ul></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"ul\" may not have text content");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testOrderedList() throws Exception {
    String input = "<messageML><ol><li>hello</li><li>world</li></ol></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element list = messageML.getChildren().get(0);

    assertEquals("Element class", OrderedList.class, list.getClass());
    assertEquals("Element tag name", "ol", list.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), list.getAttributes());
    assertEquals("Element children", 2, list.getChildren().size());
    assertEquals("Child element", ListItem.class, list.getChildren().get(0).getClass());
    assertEquals("Child element text", "hello", list.getChildren().get(0).asText());
    assertEquals("Child element", ListItem.class, list.getChildren().get(1).getClass());
    assertEquals("Child element text", "world", list.getChildren().get(1).asText());
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><ol><li>hello</li><li>world</li></ol></div>",
        context.getPresentationML());
    assertEquals("Markdown", "1. hello\n2. world\n", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML>"
        + "<ul class=\"label\"><li class=\"label\">hello</li><li>world</li></ul>"
        + "</messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    list = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, list.getAttributes().size());
    assertEquals("Attribute", "label", list.getAttribute("class"));
    Element item = list.getChildren().get(0);
    assertEquals("Attribute count", 1, item.getAttributes().size());
    assertEquals("Attribute", "label", item.getAttribute("class"));
  }

  @Test
  public void testOrderedListInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><ol style=\"label\"><li>hello</li><li>world</li></ol></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"ol\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testOrderedListItemInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><ol><li  style=\"label\">hello</li><li>world</li></ol></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"li\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testOrderedListInvalidChildren() throws Exception {
    String invalidChild = "<messageML><ol><div>Hello world!</div></ol></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"div\" is not allowed in \"ol\"");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testOrderedListTextContent() throws Exception {
    String invalidChild = "<messageML><ol>Hello world!</ol></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"ol\" may not have text content");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTable() throws Exception {
    String input = "<messageML><table>"
        + "<thead><tr><th>It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td colspan=\"6\" rowspan=\"7\">the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element table = messageML.getChildren().get(0);

    assertEquals("Element class", Table.class, table.getClass());
    assertEquals("Element tag name", "table", table.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), table.getAttributes());
    assertEquals("Element children", 3, table.getChildren().size());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\"><table>"
            + "<thead><tr><th>It</th><th>was</th></tr></thead>"
            + "<tbody><tr><td colspan=\"6\" rowspan=\"7\">the</td><td>best</td></tr></tbody>"
            + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
            + "</table></div>",
        context.getPresentationML());

    Element thead = table.getChildren().get(0);
    assertEquals("Element class", TableHeader.class, thead.getClass());
    assertEquals("Element tag name", "thead", thead.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), thead.getAttributes());
    assertEquals("Element children", 1, thead.getChildren().size());

    Element row = thead.getChildren().get(0);
    assertEquals("Element class", TableRow.class, row.getClass());
    assertEquals("Element tag name", "tr", row.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), row.getAttributes());
    assertEquals("Element children", 2, row.getChildren().size());

    Element cell = row.getChildren().get(0);
    assertEquals("Element class", TableHeaderCell.class, cell.getClass());
    assertEquals("Element tag name", "th", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    Element text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "It", ((TextNode) text).getText());

    Element tbody = table.getChildren().get(1);
    assertEquals("Element class", TableBody.class, tbody.getClass());
    assertEquals("Element tag name", "tbody", tbody.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tbody.getAttributes());
    assertEquals("Element children", 1, tbody.getChildren().size());

    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    final HashMap<String, String> expectedAttributeMap = new HashMap<>();
    expectedAttributeMap.put("colspan", "6");
    expectedAttributeMap.put("rowspan", "7");
    assertEquals("Element attributes", expectedAttributeMap, cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "the", ((TextNode) text).getText());

    Element tfoot = table.getChildren().get(2);
    assertEquals("Element class", TableFooter.class, tfoot.getClass());
    assertEquals("Element tag name", "tfoot", tfoot.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tfoot.getAttributes());
    assertEquals("Element children", 1, tfoot.getChildren().size());

    assertEquals("Markdown", "Table:\n---\nIt | was\nthe | best\nof | times\n---\n", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><table class=\"label\">"
        + "<thead class=\"label\"><tr class=\"label\"><th class=\"label\">It</th><th>was</th></tr></thead>"
        + "<tbody class=\"label\"><tr><td class=\"label\">the</td><td>best</td></tr></tbody>"
        + "<tfoot class=\"label\"><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);

    table = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, table.getAttributes().size());
    assertEquals("Attribute", "label", table.getAttribute("class"));

    thead = table.getChildren().get(0);
    assertEquals("Attribute count", 1, thead.getAttributes().size());
    assertEquals("Attribute", "label", thead.getAttribute("class"));
    row = thead.getChildren().get(0);
    assertEquals("Attribute count", 1, row.getAttributes().size());
    assertEquals("Attribute", "label", row.getAttribute("class"));
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tbody = table.getChildren().get(1);
    assertEquals("Attribute count", 1, tbody.getAttributes().size());
    assertEquals("Attribute", "label", tbody.getAttribute("class"));
    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tfoot = table.getChildren().get(2);
    assertEquals("Attribute count", 1, tfoot.getAttributes().size());
    assertEquals("Attribute", "label", tfoot.getAttribute("class"));
  }

  @Test
  public void testTableInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><table style=\"label\">"
        + "<thead><tr><th>It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td>the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"table\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableCellInvalidColSpan() throws Exception {
    String invalidAttr = "<messageML><table>"
        + "<tbody><tr><td colspan=\"6 px\">the</td><td>best</td></tr></tbody>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid input: colspan must be a int64 value not \"6 px\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableCellInvalidRowSpan() throws Exception {
    String invalidAttr = "<messageML><table>"
        + "<tbody><tr><td rowspan=\"6 px\">the</td><td>best</td></tr></tbody>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid input: rowspan must be a int64 value not \"6 px\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableInvalidChildren() throws Exception {
    String invalidChild = "<messageML><table><td>Hello world!</td></table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"td\" is not allowed in \"table\"");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableTextContent() throws Exception {
    String invalidChild = "<messageML><table>Hello world!</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"table\" may not have text content");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableHeaderInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><table>"
        + "<thead style=\"label\"><tr><th>It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td>the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"thead\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableHeaderInvalidChildren() throws Exception {
    String invalidChild = "<messageML><table><thead><td>Hello world!</td></thead></table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"td\" is not allowed in \"thead\"");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableHeaderTextContent() throws Exception {
    String invalidChild = "<messageML><table><thead>Hello world!</thead></table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"thead\" may not have text content");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableBodyInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><table>"
        + "<thead><tr><th>It</th><th>was</th></tr></thead>"
        + "<tbody style=\"label\"><tr><td>the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"tbody\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableBodyInvalidChildren() throws Exception {
    String invalidChild = "<messageML><table><tbody><td>Hello world!</td></tbody></table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"td\" is not allowed in \"tbody\"");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableBodyTextContent() throws Exception {
    String invalidChild = "<messageML><table><tbody>Hello world!</tbody></table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"tbody\" may not have text content");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableFooterInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><table>"
        + "<thead><tr><th>It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td>the</td><td>best</td></tr></tbody>"
        + "<tfoot style=\"label\"><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"tfoot\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableFooterInvalidChildren() throws Exception {
    String invalidChild = "<messageML><table><tfoot><td>Hello world!</td></tfoot></table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"td\" is not allowed in \"tfoot\"");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableFooterTextContent() throws Exception {
    String invalidChild = "<messageML><table><tfoot>Hello world!</tfoot></table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"tfoot\" may not have text content");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableRowInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><table>"
        + "<thead><tr style=\"label\"><th>It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td>the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"tr\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableRowInvalidChildren() throws Exception {
    String invalidChild = "<messageML><table><tr><div>Hello world!</div></tr></table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"div\" is not allowed in \"tr\"");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableRowTextContent() throws Exception {
    String invalidChild = "<messageML><table><tr>Hello world!</tr></table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"tr\" may not have text content");
    context.parseMessageML(invalidChild, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableHeaderCellInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><table>"
        + "<thead><tr><th style=\"label\">It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td>the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"th\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableCellInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><table>"
        + "<thead><tr><th>It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td style=\"label\">the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"td\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  private void verifyCard(Element card) throws Exception {
    assertEquals("Element class", Card.class, card.getClass());
    assertEquals("Element tag name", "card", card.getMessageMLTag());
    assertEquals("Element children", 2, card.getChildren().size());

    Element header = card.getChildren().get(0);
    assertEquals("Element class", CardHeader.class, header.getClass());
    assertEquals("Element tag name", "header", header.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), header.getAttributes());
    assertEquals("Element children", 1, header.getChildren().size());

    Element body = card.getChildren().get(1);
    assertEquals("Element class", CardBody.class, body.getClass());
    assertEquals("Element tag name", "body", body.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), body.getAttributes());
    assertEquals("Element children", 1, body.getChildren().size());

    assertEquals("Markdown", "Hello\n\nworld!\n\n", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }

  @Test
  public void testCard() throws Exception {
    String input = "<messageML><card><header>Hello</header><body>world!</body></card></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element card = messageML.getChildren().get(0);

    assertEquals("Element children", 1, messageML.getChildren().size());
    assertEquals("Element attributes", 0, card.getAttributes().size());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\"><div class=\"card\">"
            + "<div class=\"cardHeader\">Hello</div>"
            + "<div class=\"cardBody\">world!</div>"
            + "</div></div>",
        context.getPresentationML());

    verifyCard(card);

    String withAttr = "<messageML><card iconSrc=\"icon.png\" class=\"label\" accent=\"mauve\">"
        + "<header>Hello</header>"
        + "<body>world!</body>"
        + "</card></messageML>";

    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    messageML = context.getMessageML();
    card = messageML.getChildren().get(0);

    assertEquals("Element attributes", 3, card.getAttributes().size());
    assertEquals("Attribute", "label", card.getAttribute("class"));
    assertEquals("Attribute", "icon.png", card.getAttribute("iconSrc"));
    assertEquals("Attribute", "mauve", card.getAttribute("accent"));
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<div class=\"card label\" data-icon-src=\"icon.png\" data-accent-color=\"mauve\">"
        + "<div class=\"cardHeader\">Hello</div>"
        + "<div class=\"cardBody\">world!</div>"
        + "</div>"
        + "</div>", context.getPresentationML());

    verifyCard(card);
  }

  @Test
  public void testCardByPresentationML() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<div class=\"card\" data-icon-src=\"icon.png\" data-accent-color=\"mauve\">"
        + "<div class=\"cardHeader\">Hello</div>"
        + "<div class=\"cardBody\">world!</div>"
        + "</div>"
        + "</div>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element card = messageML.getChildren().get(0);

    assertEquals("Element children", 1, messageML.getChildren().size());
    assertEquals("Element attributes", 2, card.getAttributes().size());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "<div class=\"card\" data-icon-src=\"icon.png\" data-accent-color=\"mauve\">"
            + "<div class=\"cardHeader\">Hello</div>"
            + "<div class=\"cardBody\">world!</div>"
            + "</div>"
            + "</div>",
        context.getPresentationML());

    verifyCard(card);
  }

  @Test
  public void testCardInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><card style=\"label\">"
        + "<header>Hello</header>"
        + "<body>world!</body>"
        + "</card></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"style\" is not allowed in \"card\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCardHeaderInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><card>"
        + "<header class=\"label\">Hello</header>"
        + "<body>world!</body>"
        + "</card></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"header\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCardBodyInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><card>"
        + "<header>Hello</header>"
        + "<body class=\"label\">world!</body>"
        + "</card></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"body\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
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
      assertEquals("Exception message", "Element \"body\" is not allowed as a child of \"messageML\"", e.getMessage());
    }
  }

  @Test
  public void testEmojiDefaultNonRequiredAttributes() throws Exception {
    String input = "<messageML><emoji shortcode=\"smiley\"><b>Test of content</b></emoji></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element emoji = messageML.getChildren().get(0);

    assertEquals("Emoji class", Emoji.class, emoji.getClass());
    verifyEmojiPresentation((Emoji) emoji,"smiley", null, "normal","😃");
  }

  @Test
  public void testEmojiWithBlockLevelContent() throws Exception {
    String invalidContent = "<messageML><emoji shortcode=\"smiley\"><p>Invalid content</p></emoji></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"p\" is not allowed in \"emoji\"");
    context.parseMessageML(invalidContent, null, MessageML.MESSAGEML_VERSION);
    context.getMessageML();
  }

  @Test
  public void testEmojiWithNonRequiredAttributes() throws Exception {
    String input = "<messageML><emoji family=\"Rick and Morty\" size=\"big\" shortcode=\"smiley\"><b>Test of content</b></emoji></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element emoji = messageML.getChildren().get(0);

    assertEquals("Emoji class", Emoji.class, emoji.getClass());
    verifyEmojiPresentation((Emoji) emoji,"smiley", "Rick and Morty", "big","😃");
  }

  @Test
  public void testEmojiNonValidShortcode() throws Exception {
    String input = "<messageML><emoji family=\"Rick and Morty\" size=\"big\" shortcode=\"smiley.something invalid\"><b>Test of content</b></emoji></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shortcode parameter may only contain alphanumeric characters, underscore, plus sign and dash");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

  }

  @Test
  public void testEmojiUnicodeNotFound() throws Exception{
    String input = "<messageML><emoji shortcode=\"notfoundemoji\"><b>Test of content</b></emoji></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element emoji = messageML.getChildren().get(0);

    assertEquals("Emoji class", Emoji.class, emoji.getClass());
    verifyEmojiPresentation((Emoji) emoji, "notfoundemoji",null, "normal",null);
  }

  @Test
  public void testEmojiMultipleUnicodes() throws Exception{
    String input = "<messageML><emoji shortcode=\"surfer_tone3\"><b>Test of content</b></emoji></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element emoji = messageML.getChildren().get(0);

    assertEquals("Emoji class", Emoji.class, emoji.getClass());
    verifyEmojiPresentation((Emoji) emoji, "surfer_tone3",null, "normal","\uD83C\uDFC4\uD83C\uDFFD");
  }

  private void verifyEmojiPresentation(Emoji emoji, String shortcode, String family, String size, String unicode) throws JsonProcessingException {
    assertEquals("Emoji name attribute", shortcode, emoji.getShortCode());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\"><span class=\"entity\" "
        + "data-entity-id=\"emoji1\"><b>Test of content</b></span></div>", context.getPresentationML());

    String familyAttr =  (family!=null)?",\"family\":\""+family+"\"":"";
    String unicodeAttr = (unicode!=null)?",\"unicode\":\""+unicode+"\"":"";
    assertEquals("EntityJSON",
      "{"+
        "\"emoji1\":{"+
          "\"type\":\"com.symphony.emoji\","+
          "\"version\":\"1.0\","+
          "\"data\":{"+
            "\"shortcode\":\""+shortcode+"\","+
            "\"size\":\""+size+"\""+
          unicodeAttr+
          familyAttr+
          "}"+
        "}"+
      "}",
        MAPPER.writeValueAsString(context.getEntityJson()));
    assertEquals("Markdown", ":"+shortcode+":",context.getMarkdown());
  }
}
