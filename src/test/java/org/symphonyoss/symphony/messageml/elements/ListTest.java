package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class ListTest extends ElementTest {

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
    String invalidAttr = "<messageML><ul title=\"label\"><li>hello</li><li>world</li></ul></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"ul\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testBulletListItemInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><ul><li  title=\"label\">hello</li><li>world</li></ul></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"li\"");
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
    String invalidAttr = "<messageML><ol title=\"label\"><li>hello</li><li>world</li></ol></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"ol\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testOrderedListItemInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><ol><li  title=\"label\">hello</li><li>world</li></ol></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"li\"");
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
  public void testNestedBulletList() throws Exception {
    String input = "<messageML>"
        + "<ul>"
        +   "<li>top header"
        +     "<ul>"
        +       "<li>list header 1"
        +         "<ul>"
        +           "<li>item 1</li>"
        +         "</ul>"
        +       "</li>"
        +       "<li>list header 2"
        +         "<ul>"
        +           "<li>item 2</li>"
        +         "</ul>"
        +       "</li>"
        +     "</ul>"
        +   "</li>"
        + "</ul>"
        + "</messageML>";

    context.parseMessageML(input, null, null);

    String expected = "- top header\n"
        + "  - list header 1\n"
        + "    - item 1\n"
        + "  - list header 2\n"
        + "    - item 2\n";

    assertEquals("Markdown", expected, context.getMarkdown());
  }

  @Test
  public void testNestedOrderedList() throws Exception {
    String input = "<messageML>"
        + "<ol>"
        +   "<li>top header"
        +     "<ol>"
        +       "<li>list header 1"
        +         "<ol>"
        +           "<li>item 1</li>"
        +         "</ol>"
        +       "</li>"
        +       "<li>list header 2"
        +         "<ol>"
        +           "<li>item 2</li>"
        +         "</ol>"
        +       "</li>"
        +     "</ol>"
        +   "</li>"
        + "</ol>"
        + "</messageML>";

    context.parseMessageML(input, null, null);

    String expected = "1. top header\n"
        + "  1. list header 1\n"
        + "    1. item 1\n"
        + "  2. list header 2\n"
        + "    1. item 2\n";

    assertEquals("Markdown", expected, context.getMarkdown());
  }


}
