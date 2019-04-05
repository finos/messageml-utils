package org.symphonyoss.symphony.messageml.elements;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.TestDataProvider;
import org.symphonyoss.symphony.messageml.util.UserPresentation;

public class FormTest extends ElementTest{
  private static final String buttonTag = "<button type=\"action\" name=\"btn\">Button</button>";
  private static final String interactionUrl = "bot.interaction/url";

  @Before
  public void setUp() {
    super.setUp();
    UserPresentation user = new UserPresentation(1L, "bot.user1", "Bot User01",
            "bot.user1@localhost.com", interactionUrl);
    ((TestDataProvider) dataProvider).setUserPresentation(user);
  }

  @Test
  public void testCompleteForm() throws Exception {
    String id = "form-id";
    String input = "<messageML><form id=\"" + id + "\">" + buttonTag + "</form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);

    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);

    assertEquals("Form class", Form.class, form.getClass());
    verifyFormPresentation((Form) form, id);
  }

  @Test
  public void testFormWithoutActionButton() throws Exception {
    String id = "form-id";
    String resetButton = "<button type=\"reset\">Reset Button</button>";
    String input = "<messageML><form id=\"" + id + "\">" + resetButton + "</form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on form without action button");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "The form must contain at least one action button", e.getMessage());
    }
  }

  @Test
  public void testFormWithoutId() throws Exception {
    String input = "<messageML><form>" + buttonTag + "</form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on form without action button");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "The attribute \"id\" is required", e.getMessage());
    }
  }

  @Test
  public void testBadAttributeForm() throws Exception {
    String id = "form-id";
    String badAttribute = "bad-attribute";
    String input = "<messageML><form id=\"" + id + "\" " + badAttribute + "=\"wut\">" + buttonTag + "</form></messageML>";

    try {
      context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
      fail("Should have thrown an exception on form without action button");
    } catch (Exception e) {
      assertEquals("Exception class", InvalidInputException.class, e.getClass());
      assertEquals("Exception message", "Attribute \"" + badAttribute + "\" is not allowed in \"form\"", e.getMessage());
    }
  }

  private String getExpectedFormPresentation(String id) {
    return "<div data-format=\"PresentationML\" data-version=\"2.0\"><form id=\"" + id + "\" action=\"" +
            interactionUrl + "\">" + buttonTag + "</form></div>";
  }

  private String getExpectedFormMarkdown() {
    String buttonMarkdown = "(Button:Button)";
    return "\nSymphony Form (log into desktop client to answer):\n---\n" + buttonMarkdown + "\n---\n";
  }

  private void verifyFormPresentation(Form form, String id) {
    assertEquals("Form id attribute", id, form.getAttribute(Form.ID_ATTR));
    assertEquals("Form action attribute", interactionUrl, form.getAttribute(Form.ACTION_URL_ATTR));
    assertEquals("Form markdown", getExpectedFormMarkdown(), context.getMarkdown());
    assertEquals("Form presentationML", getExpectedFormPresentation(id), context.getPresentationML());
  }
}
