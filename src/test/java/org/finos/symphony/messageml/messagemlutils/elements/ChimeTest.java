package org.finos.symphony.messageml.messagemlutils.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.finos.symphony.messageml.messagemlutils.bi.BiContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

import java.util.Collections;

public class ChimeTest extends ElementTest {

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
  public void testPresentationMLShorthandChime() throws Exception {
    String invalidElement = "<div class=\"com.symphony.presentationml\"><chime/></div>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Shorthand tag \"chime\" is not allowed in PresentationML");
    context.parseMessageML(invalidElement, null, MessageML.MESSAGEML_VERSION);
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
  public void testChimeBi() throws Exception {
    String input = "<messageML><chime/></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    BiContext biContext = context.getBiContext();
    assertEquals(2, biContext.getItems().size());

    BiItem item = biContext.getItems().get(0);
    assertEquals(BiFields.CHIME.getValue(), item.getName());
    assertEquals(1, item.getAttributes().get(BiFields.COUNT.getValue()));
  }
}
