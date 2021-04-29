package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public class PreformattedTest extends ElementTest {

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


    String styleAttr =
        "<messageML><pre style=\"border-bottom:10 px;border-left-color:red\">Hello world!</pre></messageML>";
    context.parseMessageML(styleAttr, null, MessageML.MESSAGEML_VERSION);
    pre = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, pre.getAttributes().size());
    assertEquals("Attribute", "border-bottom:10 px;border-left-color:red", pre.getAttribute("style"));
  }

  @Test
  public void testPreformattedInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><pre title=\"invalid\">Hello world!</pre></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"pre\"");
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
  public void testPreBi() throws Exception {
    String input = "<messageML><pre>Hello world!</pre></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    BiContext biContext = context.getBiContext();
    assertEquals(2, biContext.getItems().size());

    BiItem item = biContext.getItems().get(0);
    assertEquals("Pres", item.getName());
    assertEquals(1, item.getAttributes().get("count"));
  }
}
