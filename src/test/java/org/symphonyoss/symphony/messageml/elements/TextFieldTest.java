package org.symphonyoss.symphony.messageml.elements;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class TextFieldTest extends ElementTest {

  private static final String NAME_ATTR = "name";
  private static final String REQUIRED_ATTR = "required";
  
  @Test
  public void testRequiredTextField() throws Exception{
    String name = "required-text-field";
    Boolean required = true;
    String input = "<messageML><form><text-field name=\"" + name + "\" required=\"" + required + "\"/></form></messageML>";
    context.parseMessageML(input, null, MessageML.MESSAGEML_VERSION);
    
    Element messageML = context.getMessageML();
    Element form = messageML.getChildren().get(0);
    Element textField = form.getChildren().get(0);
    
    assertEquals("TextField class", TextField.class, textField.getClass());
    
  }

  private String getRequiredPresentationML(String required) {
    if(required != null) {
      if(required.equals("true") || required.equals("false")) {
        return String.format(" required=\"%s\"", required);
      }
    }

    return "";
  }
  
  private String getExpectedTextFieldPresentationML(TextField textField) {
    String selectOpeningTag = "<div data-format=\"PresentationML\" data-version=\"2.0\"><form><select name=\"" +
        textField.getAttribute(NAME_ATTR) + "\"" + getRequiredPresentationML(textField.getAttribute(REQUIRED_ATTR)) + "/>";
    String selectClosingTag = "</form></div>";

    return selectOpeningTag + selectClosingTag;
  }

    private String getExpectedTextFieldMarkdown(TextField textField) {
  }
  
  
  private void verifyTextFieldPresentation(TextField textField, String name, boolean requiredAttrProvided, boolean requiredValue) {
    assertEquals("Select name attribute", name, textField.getAttribute(NAME_ATTR));
    if (requiredAttrProvided) {
      assertEquals("Select required attribute", String.valueOf(requiredValue), textField.getAttribute(REQUIRED_ATTR));
    } else {
      assertNull("Select required attribute", textField.getAttribute(REQUIRED_ATTR));
    }

    assertEquals("Select presentationML", getExpectedTextFieldPresentationML(textField), context.getPresentationML());
    assertEquals("Select markdown", getExpectedTextFieldMarkdown(textField), context.getMarkdown());
    
  }
}
