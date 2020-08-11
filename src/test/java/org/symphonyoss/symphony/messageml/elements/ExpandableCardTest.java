package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class ExpandableCardTest extends ElementTest {

  @Test
  public void testExpandableCard() throws Exception {
    String input =
        "<messageML><expandable-card state=\"collapsed\"><header>Hello</header><body>world!</body></expandable-card></messageML>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element card = messageML.getChildren().get(0);

    assertEquals("Element children", 1, messageML.getChildren().size());
    assertEquals("Element attributes", 1, card.getAttributes().size()); // state (mandatory)
    assertEquals("Attribute", "collapsed", card.getAttribute("state"));
    assertEquals("PresentationML",
        "<div data-format=\"PresentationML\" data-version=\"2.0\"><div class=\"expandable-card\" data-state=\"collapsed\">"
            + "<div class=\"expandableCardHeader\">Hello</div>"
            + "<div class=\"expandableCardBody\">world!</div>"
            + "</div></div>",
        context.getPresentationML());

    verifyExpandableCard(card);

    String withAttr = "<messageML><expandable-card state=\"collapsed\"><header>Hello</header><body variant=\"default\">world!</body></expandable-card></messageML>";

    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);
    messageML = context.getMessageML();
    card = messageML.getChildren().get(0);
    Element body = card.getChild(1);

    assertEquals("Element attributes", 1, card.getAttributes().size());
    assertEquals("Element attributes", 1, body.getAttributes().size());
    assertEquals("Attribute", "default", body.getAttribute("variant"));
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\"><div class=\"expandable-card\" data-state=\"collapsed\">"
        + "<div class=\"expandableCardHeader\">Hello</div>"
        + "<div class=\"expandableCardBody\" data-variant=\"default\">world!</div>"
        + "</div></div>", context.getPresentationML());

    verifyExpandableCard(card);
  }

  @Test
  public void testExpandableCardByPresentationML() throws Exception {
    String input = "<div data-format=\"PresentationML\" data-version=\"2.0\">"
        + "<div class=\"expandable-card\" data-state=\"collapsed\">"
        + "<div class=\"expandableCardHeader\">Hello</div>"
        + "<div class=\"expandableCardBody\">world!</div>"
        + "</div>"
        + "</div>";

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element card = messageML.getChildren().get(0);

    assertEquals("Element children", 1, messageML.getChildren().size());
    assertEquals("Element attributes", 1, card.getAttributes().size());
    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "<div class=\"expandable-card\" data-state=\"collapsed\">"
            + "<div class=\"expandableCardHeader\">Hello</div>"
            + "<div class=\"expandableCardBody\">world!</div>"
            + "</div>"
            + "</div>",
        context.getPresentationML());

    verifyExpandableCard(card);
  }

  @Test
  public void testExpandableCardInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><expandable-card title=\"label\">"
        + "<header>Hello</header>"
        + "<body>world!</body>"
        + "</expandable-card></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"expandable-card\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testExpandableCardHeaderInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><expandable-card>"
        + "<header class=\"label\">Hello</header>"
        + "<body>world!</body>"
        + "</expandable-card></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"header\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testExpandableCardBodyInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><expandable-card>"
        + "<header>Hello</header>"
        + "<body class=\"label\">world!</body>"
        + "</expandable-card></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"class\" is not allowed in \"body\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLShorthandExpandableCard() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\">"
        + "<expandable-card state=\"collapsed\">invalid</expandable-card>"
        + "</div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"expandable-card\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLShorthandExpandableCardHeader() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\">"
        + "<div class=\"expandable-card\">"
        + "<header>invalid</header>"
        + "</div>"
        + "</div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"header\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testPresentationMLShorthandExpandableCardBody() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\">"
        + "<div class=\"expandable-card\">"
        + "<div class=\"expandableCardHeader\">"
        + "<body>invalid</body>"
        + "</div>"
        + "</div>"
        + "</div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"body\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
  }

  private void verifyExpandableCard(Element card) throws Exception {
    assertEquals("Element class", ExpandableCard.class, card.getClass());
    assertEquals("Element tag name", "expandable-card", card.getMessageMLTag());
    assertEquals("Element children", 2, card.getChildren().size());

    Element header = card.getChildren().get(0);
    assertEquals("Element class", ExpandableCardHeader.class, header.getClass());
    assertEquals("Element tag name", "header", header.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), header.getAttributes());
    assertEquals("Element children", 1, header.getChildren().size());

    Element body = card.getChildren().get(1);
    assertEquals("Element class", ExpandableCardBody.class, body.getClass());
    assertEquals("Element tag name", "body", body.getMessageMLTag());
    assertEquals("Element children", 1, body.getChildren().size());

    assertEquals("Markdown", "Hello\n\nworld!\n\n", context.getMarkdown());
    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());
  }
}
