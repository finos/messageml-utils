package org.symphonyoss.symphony.messageml.bi;

public enum BiEventType {

  MESSAGEML_MESSAGE_SENT("MESSAGEML_MESSAGE_SENT"),
  MESSAGEML_ELEMENT_SENT("MESSAGEML_ELEMENT_SENT");

  private final String type;

  BiEventType(String type) {
    this.type = type;
  }

  public String getType() {
    return type;
  }
}
