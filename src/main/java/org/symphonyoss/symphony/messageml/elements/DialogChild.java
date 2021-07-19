package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

/**
 * This class is a base abstract representing all children of the Symphony Element Dialog whose tag is "dialog"
 * of type {@link Dialog}.
 * They must not have any attribute and can contain any non-interactive element except form and dialog.
 */
public abstract class DialogChild extends Element {
  public static final String DIALOG_CLASS_PREFIX = "dialog-";


  /**
   * This class represent the "title" element under the Symphony Element Dialog of tag "dialog".
   */
  public static class Title extends DialogChild {
    public static final String MESSAGEML_TAG = "title";

    public Title(Element parent, FormatEnum format) {
      super(parent, MESSAGEML_TAG, format);
    }
  }


  /**
   * This class represent the "body" element under the Symphony Element Dialog of tag "dialog".
   */
  public static class Body extends DialogChild {
    public static final String MESSAGEML_TAG = "body";

    public Body(Element parent, FormatEnum format) {
      super(parent, MESSAGEML_TAG, format);
    }
  }


  /**
   * This class represent the "footer" element under the Symphony Element Dialog of tag "dialog".
   */
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
    Collection<Class<? extends Element>> list = getValidParentClasses();
    assertNoAttributes();
    assertParent(list);



    boolean containsDialog = getChildren().stream().anyMatch(e -> e instanceof Dialog);
    if (containsDialog) {
      throw new InvalidInputException(getMessageMLTag() + " should not contain a dialog");
    }
  }

  private Collection<Class<? extends Element>> getValidParentClasses() {
    if (getParent() instanceof Form && getParent().getParent() instanceof Dialog) {
      return Collections.singletonList(Form.class);
    }
    return Collections.singletonList(Dialog.class);
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
