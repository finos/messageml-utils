package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class ListItemTest extends ElementTest {

  @Test
  public void testListItemInOrderedList() throws Exception {
    String input = "<messageML><ol><li>Item 1</li></ol></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element list = messageML.getChildren().get(0);
    Element item = list.getChildren().get(0);

    assertEquals("Ordered list class", OrderedList.class, list.getClass());
    assertEquals("List item class", ListItem.class, item.getClass());
  }

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
  public void testListItemWithoutParentList() throws Exception {
    String input = "<messageML><li>Item 1</li></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"li\" can only be a inner child of the following elements: [orderedlist, bulletlist]");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

}