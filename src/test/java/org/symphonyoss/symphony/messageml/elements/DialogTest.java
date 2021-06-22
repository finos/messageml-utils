package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.symphonyoss.symphony.messageml.elements.Dialog.MEDIUM_WIDTH;

import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.TestDataProvider;

public class DialogTest {

  private final IDataProvider dataProvider = new TestDataProvider();
  private MessageMLContext context;

  @Before
  public void setUp() {
    context = new MessageMLContext(dataProvider);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogNoId() throws Exception {
    String withContent = "<messageML><dialog/></messageML>";
    context.parseMessageML(withContent, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogEmptyId() throws Exception {
    String withContent = "<messageML><dialog id=\"\"/></messageML>";
    context.parseMessageML(withContent, null, MessageML.MESSAGEML_VERSION);
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogIdWithSpace() throws Exception {
    String withContent = "<messageML><dialog id=\"my dialog\"/></messageML>";
    context.parseMessageML(withContent, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDialogValidId() throws Exception {
    final String dialogId = "dialog-id";
    String withContent = "<messageML><dialog id=\"" + dialogId + "\"/></messageML>";

    context.parseMessageML(withContent, null, MessageML.MESSAGEML_VERSION);
    final MessageML messageML = context.getMessageML();

    assertEquals(1, messageML.getChildren().size());
    assertTrue(messageML.getChild(0) instanceof Dialog);
    assertEquals(dialogId, messageML.getChild(0).getAttribute(Element.ID_ATTR));
    assertEquals(MEDIUM_WIDTH, messageML.getChild(0).getAttribute(Dialog.WIDTH_ATTR));
  }

  @Test(expected = InvalidInputException.class)
  public void testDialogInvalidWidth() throws Exception {
    String withContent = "<messageML><dialog id=\"dialog-id\" width=\"invalid\"/></messageML>";

    context.parseMessageML(withContent, null, MessageML.MESSAGEML_VERSION);
  }

  @Test
  public void testDialogValidWidth() throws Exception {
    for (String width: Dialog.ALLOWED_WIDTH_VALUES) {
      assertDialogValidWidth(width);
    }
  }

  private void assertDialogValidWidth(String width) throws Exception {
    final String dialogId = "dialog-id";
    String withContent = "<messageML><dialog id=\"" + dialogId + "\" width=\"" + width + "\"/></messageML>";

    context.parseMessageML(withContent, null, MessageML.MESSAGEML_VERSION);
    final MessageML messageML = context.getMessageML();

    assertEquals(1, messageML.getChildren().size());
    assertTrue(messageML.getChild(0) instanceof Dialog);
    assertEquals(dialogId, messageML.getChild(0).getAttribute(Element.ID_ATTR));
    assertEquals(width, messageML.getChild(0).getAttribute(Dialog.WIDTH_ATTR));
  }
}
