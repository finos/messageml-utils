package org.finos.symphony.messageml.messagemlutils.elements;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.finos.symphony.messageml.messagemlutils.bi.BiContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

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
    expectedException.expectMessage(
        "The \"ol\" element must have at least one child that is any of the following elements: [listitem].");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testListsBi() throws Exception {
    String input = "<messageML><ol><li>Item 1</li></ol></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    BiContext biContext = context.getBiContext();
    assertEquals(2, biContext.getItems().size());

    BiItem item = biContext.getItems().get(0);
    assertEquals(BiFields.BULLET_LIST.getValue(), item.getName());
    assertEquals(1, item.getAttributes().get(BiFields.COUNT.getValue()));
  }
}
