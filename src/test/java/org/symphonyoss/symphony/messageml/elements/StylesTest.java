package org.symphonyoss.symphony.messageml.elements;

import org.junit.Test;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

public class StylesTest extends ElementTest {

  @Test
  public void validateInvalidProperty() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid property(s): [back] in the \"style\" attribute");
    Styles.validate("back:c");
  }

  @Test
  public void validateInvalidPropertyValidProperty() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Invalid property(s): [back] in the \"style\" attribute");
    Styles.validate("back:c;background:values values values;background-attachment:values");
  }

  @Test
  public void validate() throws Exception {
    expectedException.expect(InvalidInputException.class);
    expectedException.expectMessage("Unparseable \"style\" attribute: color:green;back");
    Styles.validate("color:green;back");

  }

  @Test
  public void validateAll() throws Exception {
    String validString =
        "background:values values values;background-attachment:values;background-blend-mode:values;background-clip:values;background-color:values;background-image: url('http://myimage.jpg');background-position:values;background-repeat:values;background-size:values;border:values with spaces;border-bottom:101010;"
            + "border-bottom-color:10 px;border-bottom-left-radius:values;border-bottom-right-radius:10 px values;border-bottom-style:100;border-bottom-width:values values;border-collapse:values;border-color:values;border-image:values;border-image-outset:values;border-image-repeat:values;"
            + "border-image-slice:values;border-image-source:values;border-image-width:values;border-left:values;border-left-color:values;border-left-style:values;border-left-width:values;border-radius:values;border-right:values;border-right-color:values;border-right-style:values;"
            + "border-right-width:values;border-spacing:values;border-style:values;border-top:values;border-top-color:values;border-top-left-radius:values;border-top-right-radius:values;border-top-style:values;border-top-width:values;border-width:values;box-shadow:values;box-sizing:values;"
            + "caption-side:values;clear:values;color:values;content:values;counter-increment:values;counter-reset:values;display:values;empty-cells:values;font:values;font-family:values;font-kerning:values;font-size:values;font-size-adjust:values;font-stretch:values;font-style:values;"
            + "font-variant:values;font-weight:values;height:values;letter-spacing:values;line-height:values;list-style:values;list-style-image:values;list-style-position:values;list-style-type:values;margin:values;margin-bottom:values;margin-left:values;margin-right:values;margin-top:values;"
            + "max-height:values;max-width:values;min-height:values;min-width:values;opacity:values;outline:values;outline-color:values;outline-offset:values;outline-style:values;outline-width:values;overflow:values;overflow-x:values;overflow-y:values;padding:values;padding-bottom:values;"
            + "padding-left:values;padding-right:values;padding-top:values;table-layout:values;text-align:values;text-align-last:values;text-decoration:values;text-decoration-color:values;text-decoration-line:values;text-decoration-style:values;text-indent:values;text-justify:values;"
            + "text-overflow:values;text-shadow:values;text-transform:values;visibility:values;white-space:values;width:values;word-break:values;word-spacing:values;word-wrap:values;";
    Styles.validate(validString);
  }

}