package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Collections;

public abstract class DialogChild extends Element {
  public DialogChild(Element parent, String messageMLTag, FormatEnum format) {
    super(parent, messageMLTag, format);
  }

  @Override
  public void validate() throws InvalidInputException {
    assertNoAttributes();
    assertParent(Collections.singleton(Dialog.class));
  }

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
}
