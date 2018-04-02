package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class CardTest extends ElementTest {

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
    String invalidAttr = "<messageML><card title=\"label\">"
        + "<header>Hello</header>"
        + "<body>world!</body>"
        + "</card></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"card\"");
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
  public void testPresentationMLShorthandCard() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\">"
        + "<card iconSrc=\"icon.png\" class=\"bartitle\">invalid</card>"
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
}
