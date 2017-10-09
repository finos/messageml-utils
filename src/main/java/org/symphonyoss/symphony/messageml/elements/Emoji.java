package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.EmojiNode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Node;

public class Emoji extends Entity {

  public static final String MESSAGEML_TAG = "emoji";
  public static final String ENTITY_TYPE = "org.symphonyoss.emoji";
  public static final String SUB_ENTITY_TYPE = "org.symphonyoss.emoji.name";
  private static final String ATTR_NAME = "name";
  private static final String DELIMITER = ":";
  private static final String ENTITY_VERSION = "1.0";
  private static final String ENTITY_ID_PREFIX = "emoji";

  private String name;

  public Emoji(Element parent, String name, int entityIndex) {
    this(parent, entityIndex);
    this.name = name;
  }

  public Emoji(Element parent, int entityIndex) {
    super(parent, MESSAGEML_TAG, DEFAULT_PRESENTATIONML_TAG, FormatEnum.MESSAGEML);
    this.entityId = getEntityId(entityIndex);
  }

  public String getName() {
    return this.name;
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    out.printElement(presentationMLTag, asText(), CLASS_ATTR, Entity.PRESENTATIONML_CLASS, ENTITY_ID_ATTR, entityId);
  }

  @Override
  public org.commonmark.node.Node asMarkdown() throws InvalidInputException {
    return new EmojiNode(name);
  }

  @Override
  public String asText() {
    return DELIMITER + this.name + DELIMITER;
  }

  @Override
  public void validate() throws InvalidInputException {
    if (this.name == null) {
      throw new InvalidInputException("The attribute \"name\" is required");
    }
  }

  @Override
  protected void buildAttribute(Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ATTR_NAME:
        this.name = getStringAttribute(item);
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName() + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  @Override
  protected String getEntityValue() {
    return name;
  }

  @Override
  protected String getEntitySubType() {
    return SUB_ENTITY_TYPE;
  }

  @Override
  protected String getEntityVersion() {
    return ENTITY_VERSION;
  }

  @Override
  protected String getEntityType() {
    return ENTITY_TYPE;
  }

  @Override
  protected String getEntityIdPrefix() {
    return ENTITY_ID_PREFIX;
  }
}
