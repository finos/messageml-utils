package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.util.Collections;

public abstract class DialogChild extends Element {

  public static final String DIALOG_CLASS_PREFIX = "dialog-";


  public static class Title extends DialogChild {
    public static final String MESSAGEML_TAG = "title";

    public Title(Element parent, FormatEnum format) {
      super(parent, MESSAGEML_TAG, format);
    }
  }


  public static class Body extends DialogChild {
    public static final String MESSAGEML_TAG = "body";

    public Body(Element parent, FormatEnum format) {
      super(parent, MESSAGEML_TAG, format);
    }
  }


  public static class Footer extends DialogChild {
    public static final String MESSAGEML_TAG = "footer";

    public Footer(Element parent, FormatEnum format) {
      super(parent, MESSAGEML_TAG, format);
    }
  }

  public DialogChild(Element parent, String messageMLTag, FormatEnum format) {
    super(parent, messageMLTag, format);
  }

  @Override
  public void validate() throws InvalidInputException {
    assertNoAttributes();
    assertParent(Collections.singleton(Dialog.class));

    boolean containsDialog = getChildren().stream().anyMatch(e -> e instanceof Dialog);
    if (containsDialog) {
      throw new InvalidInputException(getMessageMLTag() + " should not contain a dialog");
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out, MessageMLContext context) {
    out.openElement(Div.MESSAGEML_TAG, Collections.singletonMap(CLASS_ATTR, DIALOG_CLASS_PREFIX + getMessageMLTag()));
    for (Element child : getChildren()) {
      child.asPresentationML(out, context);
    }
    out.closeElement();
  }
}
