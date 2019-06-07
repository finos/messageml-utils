package org.symphonyoss.symphony.messageml.elements.form;

import static org.junit.Assert.assertEquals;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.ElementTest;
import org.symphonyoss.symphony.messageml.elements.Form;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.TableBody;
import org.symphonyoss.symphony.messageml.elements.TableCell;
import org.symphonyoss.symphony.messageml.elements.TableFooter;
import org.symphonyoss.symphony.messageml.elements.TableHeader;
import org.symphonyoss.symphony.messageml.elements.TableRow;
import org.symphonyoss.symphony.messageml.elements.TableSelect;
import org.symphonyoss.symphony.messageml.elements.TextNode;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;
import java.util.HashMap;

public class TableSelectTest extends ElementTest {

  @Test
  public void testTableButtonCustomHeaderAndButtonText() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" type=\"button\" header-text=\"Select It\" "
        + "position=\"left\" button-text=\"BUTTON SELECT\">"
        + "<thead><tr><td>Header 1</td><td>Header 2</td><td>Header 3</td><td>Header 4</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td><td>Content 2.1</td><td>Content 3.1</td><td>Content 4.1</td></tr>"
        + "<tr><td>Content 1.2</td><td>Content 2.2</td><td>Content 3.2</td><td>Content 4.2</td></tr>"
        + "<tr><td>Content 1.3</td><td>Content 2.3</td><td>Content 3.3</td><td>Content 4.3</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td><td>Footer 2</td><td>Footer 3</td><td>Footer 4</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element form = messageML.getChildren().get(0);

    assertEquals("Element class", Form.class, form.getClass());
    assertEquals("Element tag name", "form", form.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), form.getAttributes());
    assertEquals("Element children", 1, form.getChildren().size());

    assertEquals("Markdown", "Form (log into desktop client to answer):\n"
        + "---\n"
        + "Table Select:\n"
        + "---\n"
        + "Select It | Header 1 | Header 2 | Header 3 | Header 4\n"
        + "(Button:BUTTON SELECT) | Content 1.1 | Content 2.1 | Content 3.1 | Content 4.1\n"
        + "(Button:BUTTON SELECT) | Content 1.2 | Content 2.2 | Content 3.2 | Content 4.2\n"
        + "(Button:BUTTON SELECT) | Content 1.3 | Content 2.3 | Content 3.3 | Content 4.3\n"
        + "Footer 1 | Footer 2 | Footer 3 | Footer 4\n"
        + "---\n"
        + "\n"
        + "---\n", context.getMarkdown());

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2"
            + ".0\"><form><table><thead><tr><td>Select It</td><td>Header 1</td><td>Header "
            + "2</td><td>Header 3</td><td>Header 4</td></tr></thead><tbody><tr><td><button "
            + "name=\"tablesel-row1\"/></td><td>Content 1.1</td><td>Content 2.1</td><td>Content 3"
            + ".1</td><td>Content 4.1</td></tr><tr><td><button "
            + "name=\"tablesel-row2\"/></td><td>Content 1.2</td><td>Content 2.2</td><td>Content 3"
            + ".2</td><td>Content 4.2</td></tr><tr><td><button "
            + "name=\"tablesel-row3\"/></td><td>Content 1.3</td><td>Content 2.3</td><td>Content 3"
            + ".3</td><td>Content 4.3</td></tr></tbody><tfoot><tr></tr></tfoot></table></form></div>",
        context.getPresentationML());


    final HashMap<String, String> expectedTableSelectAttributeMap = new HashMap<>();
    expectedTableSelectAttributeMap.put("header-text", "Select It");
    expectedTableSelectAttributeMap.put("button-text", "BUTTON SELECT");
    expectedTableSelectAttributeMap.put("name", "tablesel");
    expectedTableSelectAttributeMap.put("position", "left");
    expectedTableSelectAttributeMap.put("type", "button");

    Element tableSelect = form.getChildren().get(0);
    assertEquals("Element class", TableSelect.class, tableSelect.getClass());
    assertEquals("Element tag name", "tableselect", tableSelect.getMessageMLTag());
    assertEquals("Element attributes", expectedTableSelectAttributeMap, tableSelect.getAttributes());
    assertEquals("Element children", 3, tableSelect.getChildren().size());

    Element thead = tableSelect.getChildren().get(0);
    assertEquals("Element class", TableHeader.class, thead.getClass());
    assertEquals("Element tag name", "thead", thead.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), thead.getAttributes());
    assertEquals("Element children", 1, thead.getChildren().size());

    Element row = thead.getChildren().get(0);
    assertEquals("Element class", TableRow.class, row.getClass());
    assertEquals("Element tag name", "tr", row.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), row.getAttributes());
    assertEquals("Element children", 4, row.getChildren().size());

    Element cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    Element text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Header 1", ((TextNode) text).getText());

    Element tbody = tableSelect.getChildren().get(1);
    assertEquals("Element class", TableBody.class, tbody.getClass());
    assertEquals("Element tag name", "tbody", tbody.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tbody.getAttributes());
    assertEquals("Element children", 3, tbody.getChildren().size());

    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Content 1.1", ((TextNode) text).getText());

    Element tfoot = tableSelect.getChildren().get(2);
    assertEquals("Element class", TableFooter.class, tfoot.getClass());
    assertEquals("Element tag name", "tfoot", tfoot.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tfoot.getAttributes());
    assertEquals("Element children", 1, tfoot.getChildren().size());

    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><table class=\"label\">"
        + "<thead class=\"label\"><tr class=\"label\"><th class=\"label\">It</th><th>was</th></tr></thead>"
        + "<tbody class=\"label\"><tr><td class=\"label\">the</td><td>best</td></tr></tbody>"
        + "<tfoot class=\"label\"><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);

    form = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, form.getAttributes().size());
    assertEquals("Attribute", "label", form.getAttribute("class"));

    thead = form.getChildren().get(0);
    assertEquals("Attribute count", 1, thead.getAttributes().size());
    assertEquals("Attribute", "label", thead.getAttribute("class"));
    row = thead.getChildren().get(0);
    assertEquals("Attribute count", 1, row.getAttributes().size());
    assertEquals("Attribute", "label", row.getAttribute("class"));
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tbody = form.getChildren().get(1);
    assertEquals("Attribute count", 1, tbody.getAttributes().size());
    assertEquals("Attribute", "label", tbody.getAttribute("class"));
    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tfoot = form.getChildren().get(2);
    assertEquals("Attribute count", 1, tfoot.getAttributes().size());
    assertEquals("Attribute", "label", tfoot.getAttribute("class"));
  }

  @Test
  public void testTableLeftButton() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" type=\"button\" position=\"left\">"
        + "<thead><tr><td>Header 1</td><td>Header 2</td><td>Header 3</td><td>Header 4</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td><td>Content 2.1</td><td>Content 3.1</td><td>Content 4.1</td></tr>"
        + "<tr><td>Content 1.2</td><td>Content 2.2</td><td>Content 3.2</td><td>Content 4.2</td></tr>"
        + "<tr><td>Content 1.3</td><td>Content 2.3</td><td>Content 3.3</td><td>Content 4.3</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td><td>Footer 2</td><td>Footer 3</td><td>Footer 4</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element form = messageML.getChildren().get(0);

    assertEquals("Element class", Form.class, form.getClass());
    assertEquals("Element tag name", "form", form.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), form.getAttributes());
    assertEquals("Element children", 1, form.getChildren().size());

    assertEquals("Markdown", "Form (log into desktop client to answer):\n"
        + "---\n"
        + "Table Select:\n"
        + "---\n"
        + "Select | Header 1 | Header 2 | Header 3 | Header 4\n"
        + "(Button:SELECT) | Content 1.1 | Content 2.1 | Content 3.1 | Content 4.1\n"
        + "(Button:SELECT) | Content 1.2 | Content 2.2 | Content 3.2 | Content 4.2\n"
        + "(Button:SELECT) | Content 1.3 | Content 2.3 | Content 3.3 | Content 4.3\n"
        + "Footer 1 | Footer 2 | Footer 3 | Footer 4\n"
        + "---\n"
        + "\n"
        + "---\n", context.getMarkdown());

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2"
            + ".0\"><form><table><thead><tr><td>Select</td><td>Header 1</td><td>Header "
            + "2</td><td>Header 3</td><td>Header 4</td></tr></thead><tbody><tr><td><button "
            + "name=\"tablesel-row1\"/></td><td>Content 1.1</td><td>Content 2.1</td><td>Content 3"
            + ".1</td><td>Content 4.1</td></tr><tr><td><button "
            + "name=\"tablesel-row2\"/></td><td>Content 1.2</td><td>Content 2.2</td><td>Content 3"
            + ".2</td><td>Content 4.2</td></tr><tr><td><button "
            + "name=\"tablesel-row3\"/></td><td>Content 1.3</td><td>Content 2.3</td><td>Content 3"
            + ".3</td><td>Content 4.3</td></tr></tbody><tfoot><tr></tr></tfoot></table></form></div>",
        context.getPresentationML());


    final HashMap<String, String> expectedTableSelectAttributeMap = new HashMap<>();
    expectedTableSelectAttributeMap.put("header-text", "Select");
    expectedTableSelectAttributeMap.put("button-text", "SELECT");
    expectedTableSelectAttributeMap.put("name", "tablesel");
    expectedTableSelectAttributeMap.put("position", "left");
    expectedTableSelectAttributeMap.put("type", "button");

    Element tableSelect = form.getChildren().get(0);
    assertEquals("Element class", TableSelect.class, tableSelect.getClass());
    assertEquals("Element tag name", "tableselect", tableSelect.getMessageMLTag());
    assertEquals("Element attributes", expectedTableSelectAttributeMap, tableSelect.getAttributes());
    assertEquals("Element children", 3, tableSelect.getChildren().size());

    Element thead = tableSelect.getChildren().get(0);
    assertEquals("Element class", TableHeader.class, thead.getClass());
    assertEquals("Element tag name", "thead", thead.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), thead.getAttributes());
    assertEquals("Element children", 1, thead.getChildren().size());

    Element row = thead.getChildren().get(0);
    assertEquals("Element class", TableRow.class, row.getClass());
    assertEquals("Element tag name", "tr", row.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), row.getAttributes());
    assertEquals("Element children", 4, row.getChildren().size());

    Element cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    Element text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Header 1", ((TextNode) text).getText());

    Element tbody = tableSelect.getChildren().get(1);
    assertEquals("Element class", TableBody.class, tbody.getClass());
    assertEquals("Element tag name", "tbody", tbody.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tbody.getAttributes());
    assertEquals("Element children", 3, tbody.getChildren().size());

    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Content 1.1", ((TextNode) text).getText());

    Element tfoot = tableSelect.getChildren().get(2);
    assertEquals("Element class", TableFooter.class, tfoot.getClass());
    assertEquals("Element tag name", "tfoot", tfoot.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tfoot.getAttributes());
    assertEquals("Element children", 1, tfoot.getChildren().size());

    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><table class=\"label\">"
        + "<thead class=\"label\"><tr class=\"label\"><th class=\"label\">It</th><th>was</th></tr></thead>"
        + "<tbody class=\"label\"><tr><td class=\"label\">the</td><td>best</td></tr></tbody>"
        + "<tfoot class=\"label\"><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);

    form = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, form.getAttributes().size());
    assertEquals("Attribute", "label", form.getAttribute("class"));

    thead = form.getChildren().get(0);
    assertEquals("Attribute count", 1, thead.getAttributes().size());
    assertEquals("Attribute", "label", thead.getAttribute("class"));
    row = thead.getChildren().get(0);
    assertEquals("Attribute count", 1, row.getAttributes().size());
    assertEquals("Attribute", "label", row.getAttribute("class"));
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tbody = form.getChildren().get(1);
    assertEquals("Attribute count", 1, tbody.getAttributes().size());
    assertEquals("Attribute", "label", tbody.getAttribute("class"));
    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tfoot = form.getChildren().get(2);
    assertEquals("Attribute count", 1, tfoot.getAttributes().size());
    assertEquals("Attribute", "label", tfoot.getAttribute("class"));
  }

  @Test
  public void testTableRightButton() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" type=\"button\" position=\"right\">"
        + "<thead><tr><td>Header 1</td><td>Header 2</td><td>Header 3</td><td>Header 4</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td><td>Content 2.1</td><td>Content 3.1</td><td>Content 4.1</td></tr>"
        + "<tr><td>Content 1.2</td><td>Content 2.2</td><td>Content 3.2</td><td>Content 4.2</td></tr>"
        + "<tr><td>Content 1.3</td><td>Content 2.3</td><td>Content 3.3</td><td>Content 4.3</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td><td>Footer 2</td><td>Footer 3</td><td>Footer 4</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element form = messageML.getChildren().get(0);

    assertEquals("Element class", Form.class, form.getClass());
    assertEquals("Element tag name", "form", form.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), form.getAttributes());
    assertEquals("Element children", 1, form.getChildren().size());

    assertEquals("Markdown", "Form (log into desktop client to answer):\n"
        + "---\n"
        + "Table Select:\n"
        + "---\n"
        + "Header 1 | Header 2 | Header 3 | Header 4 | Select\n"
        + "Content 1.1 | Content 2.1 | Content 3.1 | Content 4.1 | (Button:SELECT)\n"
        + "Content 1.2 | Content 2.2 | Content 3.2 | Content 4.2 | (Button:SELECT)\n"
        + "Content 1.3 | Content 2.3 | Content 3.3 | Content 4.3 | (Button:SELECT)\n"
        + "Footer 1 | Footer 2 | Footer 3 | Footer 4\n"
        + "---\n"
        + "\n"
        + "---\n", context.getMarkdown());

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2"
            + ".0\"><form><table><thead><tr><td>Header 1</td><td>Header 2</td><td>Header "
            + "3</td><td>Header 4</td><td>Select</td></tr></thead><tbody><tr><td>Content 1"
            + ".1</td><td>Content 2.1</td><td>Content 3.1</td><td>Content 4.1</td><td><button "
            + "name=\"tablesel-row1\"/></td></tr><tr><td>Content 1.2</td><td>Content 2"
            + ".2</td><td>Content 3.2</td><td>Content 4.2</td><td><button "
            + "name=\"tablesel-row2\"/></td></tr><tr><td>Content 1.3</td><td>Content 2"
            + ".3</td><td>Content 3.3</td><td>Content 4.3</td><td><button "
            + "name=\"tablesel-row3\"/></td></tr></tbody><tfoot><tr></tr></tfoot></table></form"
            + "></div>",
        context.getPresentationML());


    final HashMap<String, String> expectedTableSelectAttributeMap = new HashMap<>();
    expectedTableSelectAttributeMap.put("header-text", "Select");
    expectedTableSelectAttributeMap.put("button-text", "SELECT");
    expectedTableSelectAttributeMap.put("name", "tablesel");
    expectedTableSelectAttributeMap.put("position", "right");
    expectedTableSelectAttributeMap.put("type", "button");

    Element tableSelect = form.getChildren().get(0);
    assertEquals("Element class", TableSelect.class, tableSelect.getClass());
    assertEquals("Element tag name", "tableselect", tableSelect.getMessageMLTag());
    assertEquals("Element attributes", expectedTableSelectAttributeMap, tableSelect.getAttributes());
    assertEquals("Element children", 3, tableSelect.getChildren().size());

    Element thead = tableSelect.getChildren().get(0);
    assertEquals("Element class", TableHeader.class, thead.getClass());
    assertEquals("Element tag name", "thead", thead.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), thead.getAttributes());
    assertEquals("Element children", 1, thead.getChildren().size());

    Element row = thead.getChildren().get(0);
    assertEquals("Element class", TableRow.class, row.getClass());
    assertEquals("Element tag name", "tr", row.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), row.getAttributes());
    assertEquals("Element children", 4, row.getChildren().size());

    Element cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    Element text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Header 1", ((TextNode) text).getText());

    Element tbody = tableSelect.getChildren().get(1);
    assertEquals("Element class", TableBody.class, tbody.getClass());
    assertEquals("Element tag name", "tbody", tbody.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tbody.getAttributes());
    assertEquals("Element children", 3, tbody.getChildren().size());

    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Content 1.1", ((TextNode) text).getText());

    Element tfoot = tableSelect.getChildren().get(2);
    assertEquals("Element class", TableFooter.class, tfoot.getClass());
    assertEquals("Element tag name", "tfoot", tfoot.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tfoot.getAttributes());
    assertEquals("Element children", 1, tfoot.getChildren().size());

    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><table class=\"label\">"
        + "<thead class=\"label\"><tr class=\"label\"><th class=\"label\">It</th><th>was</th></tr></thead>"
        + "<tbody class=\"label\"><tr><td class=\"label\">the</td><td>best</td></tr></tbody>"
        + "<tfoot class=\"label\"><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);

    form = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, form.getAttributes().size());
    assertEquals("Attribute", "label", form.getAttribute("class"));

    thead = form.getChildren().get(0);
    assertEquals("Attribute count", 1, thead.getAttributes().size());
    assertEquals("Attribute", "label", thead.getAttribute("class"));
    row = thead.getChildren().get(0);
    assertEquals("Attribute count", 1, row.getAttributes().size());
    assertEquals("Attribute", "label", row.getAttribute("class"));
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tbody = form.getChildren().get(1);
    assertEquals("Attribute count", 1, tbody.getAttributes().size());
    assertEquals("Attribute", "label", tbody.getAttribute("class"));
    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tfoot = form.getChildren().get(2);
    assertEquals("Attribute count", 1, tfoot.getAttributes().size());
    assertEquals("Attribute", "label", tfoot.getAttribute("class"));
  }

  @Test
  public void testTableCheckboxCustomHeaderAndButtonText() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" type=\"checkbox\" header-text=\"Check It\" "
        + "position=\"left\" button-text=\"CHECK SELECT\">"
        + "<thead><tr><td>Header 1</td><td>Header 2</td><td>Header 3</td><td>Header 4</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td><td>Content 2.1</td><td>Content 3.1</td><td>Content 4.1</td></tr>"
        + "<tr><td>Content 1.2</td><td>Content 2.2</td><td>Content 3.2</td><td>Content 4.2</td></tr>"
        + "<tr><td>Content 1.3</td><td>Content 2.3</td><td>Content 3.3</td><td>Content 4.3</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td><td>Footer 2</td><td>Footer 3</td><td>Footer 4</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element form = messageML.getChildren().get(0);

    assertEquals("Element class", Form.class, form.getClass());
    assertEquals("Element tag name", "form", form.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), form.getAttributes());
    assertEquals("Element children", 1, form.getChildren().size());

    assertEquals("Markdown", "Form (log into desktop client to answer):\n"
        + "---\n"
        + "Table Select:\n"
        + "---\n"
        + "Check It | Header 1 | Header 2 | Header 3 | Header 4\n"
        + "(Checkbox) | Content 1.1 | Content 2.1 | Content 3.1 | Content 4.1\n"
        + "(Checkbox) | Content 1.2 | Content 2.2 | Content 3.2 | Content 4.2\n"
        + "(Checkbox) | Content 1.3 | Content 2.3 | Content 3.3 | Content 4.3\n"
        + "Footer 1 | Footer 2 | Footer 3 | Footer 4\n"
        + "---\n"
        + "\n"
        + "---\n", context.getMarkdown());

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2"
            + ".0\"><form><table><thead><tr><td><input type=\"checkbox\" "
            + "name=\"tablesel-header\"/></td><td>Header 1</td><td>Header 2</td><td>Header "
            + "3</td><td>Header 4</td></tr></thead><tbody><tr><td><input type=\"checkbox\" "
            + "name=\"tablesel-row1\"/></td><td>Content 1.1</td><td>Content 2.1</td><td>Content 3"
            + ".1</td><td>Content 4.1</td></tr><tr><td><input type=\"checkbox\" "
            + "name=\"tablesel-row2\"/></td><td>Content 1.2</td><td>Content 2.2</td><td>Content 3"
            + ".2</td><td>Content 4.2</td></tr><tr><td><input type=\"checkbox\" "
            + "name=\"tablesel-row3\"/></td><td>Content 1.3</td><td>Content 2.3</td><td>Content 3"
            + ".3</td><td>Content 4.3</td></tr></tbody><tfoot><tr></tr></tfoot></table></form></div>",
        context.getPresentationML());


    final HashMap<String, String> expectedTableSelectAttributeMap = new HashMap<>();
    expectedTableSelectAttributeMap.put("header-text", "Check It");
    expectedTableSelectAttributeMap.put("button-text", "CHECK SELECT");
    expectedTableSelectAttributeMap.put("name", "tablesel");
    expectedTableSelectAttributeMap.put("position", "left");
    expectedTableSelectAttributeMap.put("type", "checkbox");

    Element tableSelect = form.getChildren().get(0);
    assertEquals("Element class", TableSelect.class, tableSelect.getClass());
    assertEquals("Element tag name", "tableselect", tableSelect.getMessageMLTag());
    assertEquals("Element attributes", expectedTableSelectAttributeMap, tableSelect.getAttributes());
    assertEquals("Element children", 3, tableSelect.getChildren().size());

    Element thead = tableSelect.getChildren().get(0);
    assertEquals("Element class", TableHeader.class, thead.getClass());
    assertEquals("Element tag name", "thead", thead.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), thead.getAttributes());
    assertEquals("Element children", 1, thead.getChildren().size());

    Element row = thead.getChildren().get(0);
    assertEquals("Element class", TableRow.class, row.getClass());
    assertEquals("Element tag name", "tr", row.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), row.getAttributes());
    assertEquals("Element children", 4, row.getChildren().size());

    Element cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    Element text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Header 1", ((TextNode) text).getText());

    Element tbody = tableSelect.getChildren().get(1);
    assertEquals("Element class", TableBody.class, tbody.getClass());
    assertEquals("Element tag name", "tbody", tbody.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tbody.getAttributes());
    assertEquals("Element children", 3, tbody.getChildren().size());

    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Content 1.1", ((TextNode) text).getText());

    Element tfoot = tableSelect.getChildren().get(2);
    assertEquals("Element class", TableFooter.class, tfoot.getClass());
    assertEquals("Element tag name", "tfoot", tfoot.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tfoot.getAttributes());
    assertEquals("Element children", 1, tfoot.getChildren().size());

    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><table class=\"label\">"
        + "<thead class=\"label\"><tr class=\"label\"><th class=\"label\">It</th><th>was</th></tr></thead>"
        + "<tbody class=\"label\"><tr><td class=\"label\">the</td><td>best</td></tr></tbody>"
        + "<tfoot class=\"label\"><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);

    form = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, form.getAttributes().size());
    assertEquals("Attribute", "label", form.getAttribute("class"));

    thead = form.getChildren().get(0);
    assertEquals("Attribute count", 1, thead.getAttributes().size());
    assertEquals("Attribute", "label", thead.getAttribute("class"));
    row = thead.getChildren().get(0);
    assertEquals("Attribute count", 1, row.getAttributes().size());
    assertEquals("Attribute", "label", row.getAttribute("class"));
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tbody = form.getChildren().get(1);
    assertEquals("Attribute count", 1, tbody.getAttributes().size());
    assertEquals("Attribute", "label", tbody.getAttribute("class"));
    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tfoot = form.getChildren().get(2);
    assertEquals("Attribute count", 1, tfoot.getAttributes().size());
    assertEquals("Attribute", "label", tfoot.getAttribute("class"));
  }

  @Test
  public void testTableLeftCheckBox() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" type=\"checkbox\" position=\"left\">"
        + "<thead><tr><td>Header 1</td><td>Header 2</td><td>Header 3</td><td>Header 4</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td><td>Content 2.1</td><td>Content 3.1</td><td>Content 4.1</td></tr>"
        + "<tr><td>Content 1.2</td><td>Content 2.2</td><td>Content 3.2</td><td>Content 4.2</td></tr>"
        + "<tr><td>Content 1.3</td><td>Content 2.3</td><td>Content 3.3</td><td>Content 4.3</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td><td>Footer 2</td><td>Footer 3</td><td>Footer 4</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element form = messageML.getChildren().get(0);

    assertEquals("Element class", Form.class, form.getClass());
    assertEquals("Element tag name", "form", form.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), form.getAttributes());
    assertEquals("Element children", 1, form.getChildren().size());

    assertEquals("Markdown", "Form (log into desktop client to answer):\n"
        + "---\n"
        + "Table Select:\n"
        + "---\n"
        + "Select | Header 1 | Header 2 | Header 3 | Header 4\n"
        + "(Checkbox) | Content 1.1 | Content 2.1 | Content 3.1 | Content 4.1\n"
        + "(Checkbox) | Content 1.2 | Content 2.2 | Content 3.2 | Content 4.2\n"
        + "(Checkbox) | Content 1.3 | Content 2.3 | Content 3.3 | Content 4.3\n"
        + "Footer 1 | Footer 2 | Footer 3 | Footer 4\n"
        + "---\n"
        + "\n"
        + "---\n", context.getMarkdown());

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2"
            + ".0\"><form><table><thead><tr><td><input type=\"checkbox\" "
            + "name=\"tablesel-header\"/></td><td>Header 1</td><td>Header 2</td><td>Header "
            + "3</td><td>Header 4</td></tr></thead><tbody><tr><td><input type=\"checkbox\" "
            + "name=\"tablesel-row1\"/></td><td>Content 1.1</td><td>Content 2.1</td><td>Content 3"
            + ".1</td><td>Content 4.1</td></tr><tr><td><input type=\"checkbox\" "
            + "name=\"tablesel-row2\"/></td><td>Content 1.2</td><td>Content 2.2</td><td>Content 3"
            + ".2</td><td>Content 4.2</td></tr><tr><td><input type=\"checkbox\" "
            + "name=\"tablesel-row3\"/></td><td>Content 1.3</td><td>Content 2.3</td><td>Content 3"
            + ".3</td><td>Content 4.3</td></tr></tbody><tfoot><tr></tr></tfoot></table></form></div>",
        context.getPresentationML());


    final HashMap<String, String> expectedTableSelectAttributeMap = new HashMap<>();
    expectedTableSelectAttributeMap.put("header-text", "Select");
    expectedTableSelectAttributeMap.put("button-text", "SELECT");
    expectedTableSelectAttributeMap.put("name", "tablesel");
    expectedTableSelectAttributeMap.put("position", "left");
    expectedTableSelectAttributeMap.put("type", "checkbox");

    Element tableSelect = form.getChildren().get(0);
    assertEquals("Element class", TableSelect.class, tableSelect.getClass());
    assertEquals("Element tag name", "tableselect", tableSelect.getMessageMLTag());
    assertEquals("Element attributes", expectedTableSelectAttributeMap, tableSelect.getAttributes());
    assertEquals("Element children", 3, tableSelect.getChildren().size());

    Element thead = tableSelect.getChildren().get(0);
    assertEquals("Element class", TableHeader.class, thead.getClass());
    assertEquals("Element tag name", "thead", thead.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), thead.getAttributes());
    assertEquals("Element children", 1, thead.getChildren().size());

    Element row = thead.getChildren().get(0);
    assertEquals("Element class", TableRow.class, row.getClass());
    assertEquals("Element tag name", "tr", row.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), row.getAttributes());
    assertEquals("Element children", 4, row.getChildren().size());

    Element cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    Element text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Header 1", ((TextNode) text).getText());

    Element tbody = tableSelect.getChildren().get(1);
    assertEquals("Element class", TableBody.class, tbody.getClass());
    assertEquals("Element tag name", "tbody", tbody.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tbody.getAttributes());
    assertEquals("Element children", 3, tbody.getChildren().size());

    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Content 1.1", ((TextNode) text).getText());

    Element tfoot = tableSelect.getChildren().get(2);
    assertEquals("Element class", TableFooter.class, tfoot.getClass());
    assertEquals("Element tag name", "tfoot", tfoot.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tfoot.getAttributes());
    assertEquals("Element children", 1, tfoot.getChildren().size());

    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><table class=\"label\">"
        + "<thead class=\"label\"><tr class=\"label\"><th class=\"label\">It</th><th>was</th></tr></thead>"
        + "<tbody class=\"label\"><tr><td class=\"label\">the</td><td>best</td></tr></tbody>"
        + "<tfoot class=\"label\"><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);

    form = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, form.getAttributes().size());
    assertEquals("Attribute", "label", form.getAttribute("class"));

    thead = form.getChildren().get(0);
    assertEquals("Attribute count", 1, thead.getAttributes().size());
    assertEquals("Attribute", "label", thead.getAttribute("class"));
    row = thead.getChildren().get(0);
    assertEquals("Attribute count", 1, row.getAttributes().size());
    assertEquals("Attribute", "label", row.getAttribute("class"));
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tbody = form.getChildren().get(1);
    assertEquals("Attribute count", 1, tbody.getAttributes().size());
    assertEquals("Attribute", "label", tbody.getAttribute("class"));
    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tfoot = form.getChildren().get(2);
    assertEquals("Attribute count", 1, tfoot.getAttributes().size());
    assertEquals("Attribute", "label", tfoot.getAttribute("class"));
  }

  @Test
  public void testTableRightCheckbox() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" type=\"checkbox\" position=\"right\">"
        + "<thead><tr><td>Header 1</td><td>Header 2</td><td>Header 3</td><td>Header 4</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td><td>Content 2.1</td><td>Content 3.1</td><td>Content 4.1</td></tr>"
        + "<tr><td>Content 1.2</td><td>Content 2.2</td><td>Content 3.2</td><td>Content 4.2</td></tr>"
        + "<tr><td>Content 1.3</td><td>Content 2.3</td><td>Content 3.3</td><td>Content 4.3</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td><td>Footer 2</td><td>Footer 3</td><td>Footer 4</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    assertEquals("Element children", 1, messageML.getChildren().size());

    Element form = messageML.getChildren().get(0);

    assertEquals("Element class", Form.class, form.getClass());
    assertEquals("Element tag name", "form", form.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), form.getAttributes());
    assertEquals("Element children", 1, form.getChildren().size());

    assertEquals("Markdown", "Form (log into desktop client to answer):\n"
        + "---\n"
        + "Table Select:\n"
        + "---\n"
        + "Header 1 | Header 2 | Header 3 | Header 4 | Select\n"
        + "Content 1.1 | Content 2.1 | Content 3.1 | Content 4.1 | (Checkbox)\n"
        + "Content 1.2 | Content 2.2 | Content 3.2 | Content 4.2 | (Checkbox)\n"
        + "Content 1.3 | Content 2.3 | Content 3.3 | Content 4.3 | (Checkbox)\n"
        + "Footer 1 | Footer 2 | Footer 3 | Footer 4\n"
        + "---\n"
        + "\n"
        + "---\n", context.getMarkdown());

    assertEquals("PresentationML", "<div data-format=\"PresentationML\" data-version=\"2"
            + ".0\"><form><table><thead><tr><td>Header 1</td><td>Header 2</td><td>Header "
            + "3</td><td>Header 4</td><td><input type=\"checkbox\" "
            + "name=\"tablesel-header\"/></td></tr></thead><tbody><tr><td>Content 1"
            + ".1</td><td>Content 2.1</td><td>Content 3.1</td><td>Content 4.1</td><td><input "
            + "type=\"checkbox\" name=\"tablesel-row1\"/></td></tr><tr><td>Content 1"
            + ".2</td><td>Content 2.2</td><td>Content 3.2</td><td>Content 4.2</td><td><input "
            + "type=\"checkbox\" name=\"tablesel-row2\"/></td></tr><tr><td>Content 1"
            + ".3</td><td>Content 2.3</td><td>Content 3.3</td><td>Content 4.3</td><td><input "
            + "type=\"checkbox\" name=\"tablesel-row3\"/></td></tr></tbody><tfoot><tr></tr"
            + "></tfoot></table></form></div>",
        context.getPresentationML());


    final HashMap<String, String> expectedTableSelectAttributeMap = new HashMap<>();
    expectedTableSelectAttributeMap.put("header-text", "Select");
    expectedTableSelectAttributeMap.put("button-text", "SELECT");
    expectedTableSelectAttributeMap.put("name", "tablesel");
    expectedTableSelectAttributeMap.put("position", "right");
    expectedTableSelectAttributeMap.put("type", "checkbox");

    Element tableSelect = form.getChildren().get(0);
    assertEquals("Element class", TableSelect.class, tableSelect.getClass());
    assertEquals("Element tag name", "tableselect", tableSelect.getMessageMLTag());
    assertEquals("Element attributes", expectedTableSelectAttributeMap, tableSelect.getAttributes());
    assertEquals("Element children", 3, tableSelect.getChildren().size());

    Element thead = tableSelect.getChildren().get(0);
    assertEquals("Element class", TableHeader.class, thead.getClass());
    assertEquals("Element tag name", "thead", thead.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), thead.getAttributes());
    assertEquals("Element children", 1, thead.getChildren().size());

    Element row = thead.getChildren().get(0);
    assertEquals("Element class", TableRow.class, row.getClass());
    assertEquals("Element tag name", "tr", row.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), row.getAttributes());
    assertEquals("Element children", 4, row.getChildren().size());

    Element cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    Element text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Header 1", ((TextNode) text).getText());

    Element tbody = tableSelect.getChildren().get(1);
    assertEquals("Element class", TableBody.class, tbody.getClass());
    assertEquals("Element tag name", "tbody", tbody.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tbody.getAttributes());
    assertEquals("Element children", 3, tbody.getChildren().size());

    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Element class", TableCell.class, cell.getClass());
    assertEquals("Element tag name", "td", cell.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), cell.getAttributes());
    assertEquals("Element children", 1, cell.getChildren().size());

    text = cell.getChildren().get(0);
    assertEquals("Element class", TextNode.class, text.getClass());
    assertEquals("Element text", "Content 1.1", ((TextNode) text).getText());

    Element tfoot = tableSelect.getChildren().get(2);
    assertEquals("Element class", TableFooter.class, tfoot.getClass());
    assertEquals("Element tag name", "tfoot", tfoot.getMessageMLTag());
    assertEquals("Element attributes", Collections.emptyMap(), tfoot.getAttributes());
    assertEquals("Element children", 1, tfoot.getChildren().size());

    assertEquals("EntityJSON", new ObjectNode(JsonNodeFactory.instance), context.getEntityJson());
    assertEquals("Legacy entities", new ObjectNode(JsonNodeFactory.instance), context.getEntities());

    String withAttr = "<messageML><table class=\"label\">"
        + "<thead class=\"label\"><tr class=\"label\"><th class=\"label\">It</th><th>was</th></tr></thead>"
        + "<tbody class=\"label\"><tr><td class=\"label\">the</td><td>best</td></tr></tbody>"
        + "<tfoot class=\"label\"><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    context.parseMessageML(withAttr, null, MessageML.MESSAGEML_VERSION);

    form = context.getMessageML().getChildren().get(0);
    assertEquals("Attribute count", 1, form.getAttributes().size());
    assertEquals("Attribute", "label", form.getAttribute("class"));

    thead = form.getChildren().get(0);
    assertEquals("Attribute count", 1, thead.getAttributes().size());
    assertEquals("Attribute", "label", thead.getAttribute("class"));
    row = thead.getChildren().get(0);
    assertEquals("Attribute count", 1, row.getAttributes().size());
    assertEquals("Attribute", "label", row.getAttribute("class"));
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tbody = form.getChildren().get(1);
    assertEquals("Attribute count", 1, tbody.getAttributes().size());
    assertEquals("Attribute", "label", tbody.getAttribute("class"));
    row = tbody.getChildren().get(0);
    cell = row.getChildren().get(0);
    assertEquals("Attribute count", 1, cell.getAttributes().size());
    assertEquals("Attribute", "label", cell.getAttribute("class"));

    tfoot = form.getChildren().get(2);
    assertEquals("Attribute count", 1, tfoot.getAttributes().size());
    assertEquals("Attribute", "label", tfoot.getAttribute("class"));
  }

  @Test
  public void testTableSelectWithoutName() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect type=\"button\" position=\"left\">"
        + "<thead><tr><td>Header 1</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableSelectEmptyName() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"\" type=\"button\" position=\"left\">"
        + "<thead><tr><td>Header 1</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableSelectEmptyTrimName() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"  \" type=\"button\" position=\"left\">"
        + "<thead><tr><td>Header 1</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"name\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableSelectWithoutType() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" position=\"left\">"
        + "<thead><tr><td>Header 1</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"type\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableSelectEmptyType() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" type=\"\" position=\"left\">"
        + "<thead><tr><td>Header 1</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"type\" must be \"button\" or \"checkbox\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableSelectInvalidType() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" type=\"text\" position=\"left\">"
        + "<thead><tr><td>Header 1</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"type\" must be \"button\" or \"checkbox\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableSelectWithoutPosition() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" type=\"button\">"
        + "<thead><tr><td>Header 1</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The attribute \"position\" is required");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableSelectEmptyPosition() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" type=\"button\" position=\"\">"
        + "<thead><tr><td>Header 1</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"position\" must be \"left\" or \"right\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableSelectInvalidPosition() throws Exception {
    String input = "<messageML>"
        + "<form>"
        + "<tableselect name=\"tablesel\" type=\"button\" position=\"middle\">"
        + "<thead><tr><td>Header 1</td></tr></thead>"
        + "<tbody>"
        + "<tr><td>Content 1.1</td></tr>"
        + "</tbody>"
        + "<tfoot><tr><td>Footer 1</td></tr></tfoot>"
        + "</tableselect>"
        + "</form></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"position\" must be \"left\" or \"right\"");

    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

}
