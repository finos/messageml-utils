package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

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
}
