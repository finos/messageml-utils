package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

public class TableTest extends ElementTest {

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
  public void testParseMarkdownWithOnlyTable() throws Exception {
    String markdown = "";
    ObjectNode entities = new ObjectNode(JsonNodeFactory.instance);

    ObjectNode media = new ObjectNode(JsonNodeFactory.instance);
    media.put("mediaType", "JSON");

    ArrayNode contentWrapper = new ArrayNode(JsonNodeFactory.instance);
    ObjectNode content = new ObjectNode(JsonNodeFactory.instance);
    content.put("index", 0);
    content.put("type", "excel-rcp");

    ArrayNode text = new ArrayNode(JsonNodeFactory.instance);
    ArrayNode r1 = new ArrayNode(JsonNodeFactory.instance);
    r1.add("A1");
    r1.add("B1");
    ArrayNode r2 = new ArrayNode(JsonNodeFactory.instance);
    r2.add("A2");
    r2.add("B2");

    text.add(r1);
    text.add(r2);
    content.set("text", text);
    contentWrapper.add(content);
    media.set("content", content);

    context.parseMarkdown(markdown, entities, media);

    assertEquals("Generated PresentationML", "<div data-format=\"PresentationML\" data-version=\"2.0\">"
            + "<table><tr><td>A1</td><td>B1</td></tr><tr><td>A2</td><td>B2</td></tr></table></div>",
        context.getPresentationML());
    assertEquals("Generated Markdown", "Table:\n"
            + "---\n"
            + "A1 | B1\n"
            + "A2 | B2\n"
            + "---\n",
        context.getMarkdown());
  }

  @Test
  public void testTableInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><table title=\"label\">"
        + "<thead><tr><th>It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td>the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"table\"");
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
        + "<thead title=\"label\"><tr><th>It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td>the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"thead\"");
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
        + "<tbody title=\"label\"><tr><td>the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"tbody\"");
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
        + "<tfoot title=\"label\"><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"tfoot\"");
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
        + "<thead><tr title=\"label\"><th>It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td>the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"tr\"");
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
        + "<thead><tr><th title=\"label\">It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td>the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"th\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testTableCellInvalidAttr() throws Exception {
    String invalidAttr = "<messageML><table>"
        + "<thead><tr><th>It</th><th>was</th></tr></thead>"
        + "<tbody><tr><td title=\"label\">the</td><td>best</td></tr></tbody>"
        + "<tfoot><tr><th>of</th><td>times</td></tr></tfoot>"
        + "</table></messageML>";
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Attribute \"title\" is not allowed in \"td\"");
    context.parseMessageML(invalidAttr, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testCompleteTableBi() throws Exception {
    String input = "<messageML>" +
        "  <table>" +
        "    <tbody>" +
        "      <tr><td colspan=\"3\">Content 1.1 with colspan</td><td>Content 3.1</td></tr>" +
        "      <tr><td>element 1</td><td>element 2</td><td>element 3</td><td>element 4</td><td>element 5</td><td>element 6</td></tr>"
        +
        "    </tbody>" +
        "    <tfoot>" +
        "      <tr><td>Footer 1</td><td>Footer 2</td><td>Footer 3</td></tr>" +
        "    </tfoot>" +
        "  </table>" +
        "  <table>" +
        "    <thead>" +
        "      <tr><td>Header 1</td><td>Header 2</td><td>Header 3</td><td>Header 4</td></tr>" +
        "    </thead>" +
        "    <tbody>" +
        "      <tr><td colspan=\"2\">Content 1.1 with colspan</td><td>Content 3.1</td><td>Content 4.1</td></tr>" +
        "      <tr><td rowspan=\"2\">Content 1.2 with rowspan</td><td>Content 2.2</td><td>Content 3.2</td><td>Content 4.2</td></tr>"
        +
        "      <tr><td>Content 2.3</td><td>Content 3.3</td><td>Content 4.3</td></tr>" +
        "    </tbody>" +
        "    <tfoot>" +
        "      <tr><td>Footer 1</td><td>Footer 2</td><td>Footer 3</td><td>Footer 4</td></tr>" +
        "    </tfoot>" +
        "  </table>" +
        "</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    List<BiItem> expectedBiItems = getExpectedTableBiItems();

    List<BiItem> biItems = context.getBiContext().getItems();
    assertEquals(biItems.size(), expectedBiItems.size());
    assertTrue(biItems.containsAll(expectedBiItems));
    assertTrue(expectedBiItems.containsAll(biItems));
  }

  private List<BiItem> getExpectedTableBiItems() {
    List<BiItem> biItems = new ArrayList<>();
    biItems.add(new BiItem(BiFields.TABLE.getFieldName(), Collections.singletonMap(BiFields.COUNT.getFieldName(), 2)));
    biItems.add(new BiItem(BiFields.TABLE_COLUMN_MAX.getFieldName(), Collections.singletonMap(BiFields.COUNT.getFieldName(), 6)));
    biItems.add(new BiItem(BiFields.TABLE_ROW_MAX.getFieldName(), Collections.singletonMap(BiFields.COUNT.getFieldName(), 5)));
    biItems.add(new BiItem(BiFields.TABLE_CELL_COL_SPAN.getFieldName(), Collections.singletonMap(BiFields.COUNT.getFieldName(), 2)));
    biItems.add(new BiItem(BiFields.TABLE_CELL_ROW_SPAN.getFieldName(), Collections.singletonMap(BiFields.COUNT.getFieldName(), 1)));
    biItems.add(new BiItem(BiFields.TABLE_HEADER.getFieldName(), Collections.singletonMap(BiFields.COUNT.getFieldName(), 1)));
    biItems.add(new BiItem(BiFields.TABLE_FOOTER.getFieldName(), Collections.singletonMap(BiFields.COUNT.getFieldName(), 2)));
    biItems.add(new BiItem(BiFields.MESSAGE_LENGTH.getFieldName(), Collections.singletonMap(BiFields.COUNT.getFieldName(), 906)));
    return biItems;
  }

}
