package org.symphonyoss.symphony.messageml.elements;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.NoOpDataProvider;
import org.symphonyoss.symphony.messageml.util.NullDataProvider;

import static org.junit.Assert.assertEquals;

public class LinkTest extends ElementTest {

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
    String invalidAttr = "<messageML>Hello<a title=\"label\">world</a>!</messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"a\"");
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

  @Test
  public void testLinkNullDataProvider() throws Exception {
    String invalidUri = "<messageML><a href=\"invalid://hello.org\">Hello world!</a></messageML>";
    IDataProvider nullProvider = new NullDataProvider();
    MessageMLContext context = new MessageMLContext(nullProvider);

    context.parseMessageML(invalidUri, null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();
    ObjectNode entityJson = context.getEntityJson();
    String markdown = context.getMarkdown();
    String text = context.getText();

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\"><a href=\"invalid://hello.org\">Hello world!</a></div>";
    String expectedJson = "{}";
    String expectedMarkdown = "invalid://hello.org";
    String expectedText = "Hello world!";

    assertEquals("Generated PresentationML", expectedPresentationML, presentationML);
    assertEquals("Generated EntityJSON", expectedJson, MAPPER.writeValueAsString(entityJson));
    assertEquals("Generated Markdown", expectedMarkdown, markdown);
    assertEquals("Generated text", expectedText, text);
  }

  @Test
  public void testLinkNoOpDataProvider() throws Exception {
    String invalidUri = "<messageML><a href=\"invalid://hello.org\">Hello world!</a></messageML>";
    IDataProvider noOpProvider = new NoOpDataProvider();
    MessageMLContext context = new MessageMLContext(noOpProvider);

    context.parseMessageML(invalidUri, null, MessageML.MESSAGEML_VERSION);

    String presentationML = context.getPresentationML();
    ObjectNode entityJson = context.getEntityJson();
    String markdown = context.getMarkdown();
    String text = context.getText();

    String expectedPresentationML = "<div data-format=\"PresentationML\" data-version=\"2.0\"><a href=\"invalid://hello.org\">Hello world!</a></div>";
    String expectedJson = "{}";
    String expectedMarkdown = "invalid://hello.org";
    String expectedText = "Hello world!";

    assertEquals("Generated PresentationML", expectedPresentationML, presentationML);
    assertEquals("Generated EntityJSON", expectedJson, MAPPER.writeValueAsString(entityJson));
    assertEquals("Generated Markdown", expectedMarkdown, markdown);
    assertEquals("Generated text", expectedText, text);
  }

  @Test
  public void testLinkBi() throws Exception {
    String input = "<messageML><a href=\"https://hello.org\">Hello world!</a></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    BiContext biContext = context.getBiContext();
    assertEquals(2, biContext.getItems().size());

    BiItem item = biContext.getItems().get(0);
    assertEquals("Links", item.getName());
    assertEquals(1, item.getAttributes().get("count"));
  }
}
