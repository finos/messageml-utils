package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.bi.BiContext;
import org.symphonyoss.symphony.messageml.bi.BiFields;
import org.symphonyoss.symphony.messageml.bi.BiItem;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class BulletListTest extends ElementTest {

  @Test
  public void testListItemInBulletList() throws Exception {
    String input = "<messageML><ul><li>Item 1</li></ul></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element list = messageML.getChildren().get(0);
    Element item = list.getChildren().get(0);

    assertEquals("Bullet list class", BulletList.class, list.getClass());
    assertEquals("List item class", ListItem.class, item.getClass());
  }

  @Test
  public void testWithoutListItemAsChild() throws Exception {
    String input = "<messageML><ul></ul></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage(
        "The \"ul\" element must have at least one child that is any of the following elements: [listitem].");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testListsBi() throws Exception {
    String input = "<messageML>" +
        "<ol><li>ItemList1</li></ol>" +
        "<ul><li>ItemList2</li></ul>" +
        "<ul><li>ItemList3</li></ul>" +
        "</messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    BiContext biContext = context.getBiContext();
    assertEquals(2, biContext.getItems().size());

    BiItem item = biContext.getItems().get(0);
    assertEquals(BiFields.BULLET_LIST.getFieldName(), item.getName());
    assertEquals(3, item.getAttributes().get(BiFields.COUNT.getFieldName()));
  }

}
