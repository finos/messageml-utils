package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.*;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class OrderedListTest extends ElementTest {

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
  public void testWithoutListItemAsChild() throws Exception {
    String input = "<messageML><ol></ol></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("The \"ol\" element must have at least one child that is any of the following elements: [listitem].");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

}