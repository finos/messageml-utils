package org.finos.symphony.messageml.messagemlutils.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ImageTest extends ElementTest {

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
    String invalidAttr = "<messageML><img title=\"label\" src=\"hello.png\"/></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"img\"");
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
  public void testImageBi() throws Exception {
    String input = "<messageML><img src=\"https://yourimg.com/test/myimage.svg\"/>" +
        "<img src=\"data:image/svg+xml;base64,PHN2ZyBpZD0i...DcuMjcsMTYuN=\"/>" +
        "<img src=\"https://yourimg.com/test/anotherimage.svg\"/>" +
        "</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    List<BiItem> expectedBiItems = getExpectedImageBiItems();

    List<BiItem> biItems = context.getBiContext().getItems();
    assertEquals(biItems.size(), expectedBiItems.size());
    assertTrue(biItems.containsAll(expectedBiItems));
    assertTrue(expectedBiItems.containsAll(biItems));
  }

  private List<BiItem> getExpectedImageBiItems() {
    List<BiItem> biItems = new ArrayList<>();
    biItems.add(new BiItem(BiFields.IMAGE.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 3)));
    biItems.add(new BiItem(BiFields.IMAGE_URL.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 2)));
    biItems.add(new BiItem(BiFields.IMAGE_DATA.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 1)));
    biItems.add(new BiItem(
        BiFields.MESSAGE_LENGTH.getValue(), Collections.singletonMap(BiFields.COUNT.getValue(), 193)));
    return biItems;
  }
}
