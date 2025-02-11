package org.finos.symphony.messageml.messagemlutils.elements;

import org.junit.Test;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

public class ListItemTest extends ElementTest {

  @Test
  public void testListItemWithoutParentList() throws Exception {
    String input = "<messageML><li>Item 1</li></messageML>";

    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Element \"li\" can only be a child of the following elements: [orderedlist, bulletlist]");
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
  }

}
