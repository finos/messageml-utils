package org.finos.symphony.messageml.messagemlutils.bi;

public enum BiEventType {

  MESSAGEML_MESSAGE_SENT("MESSAGEML_MESSAGE_SENT"),
  MESSAGEML_ELEMENT_SENT("MESSAGEML_ELEMENT_SENT"),
  NONE("NONE");

  private final String type;

  BiEventType(String type) {
    this.type = type;
  }

  public String getType() {
    return type;
  }
}
