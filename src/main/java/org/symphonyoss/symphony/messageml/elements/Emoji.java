package org.symphonyoss.symphony.messageml.elements;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.EmojiNode;
import org.symphonyoss.symphony.messageml.util.EmojiAnnotationToUnicode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Node;

public class Emoji extends Entity {

  public static final String MESSAGEML_TAG = "emoji";
  private static final String ATTR_ANNOTATION = "annotation";
  private static final String ATTR_FAMILY = "family";
  private static final String ATTR_SIZE = "size";

  private static final String ENTITY_TYPE = "com.symphony.emoji";
  private static final String ENTITY_VERSION = "1.0";
  private static final String ENTITY_ID_PREFIX = "emoji";
  private static final String DATA_FIELD = "data";
  private static final String UNICODE_FIELD = "unicode";
  private static final String DEFAULT_EMOJI_SIZE = "normal";

  private String annotation;
  private String family;
  private String size;

  public Emoji(Element parent, String annotation, int entityIndex) {
    this(parent, entityIndex);
    this.annotation = annotation;
  }

  public Emoji(Element parent, int entityIndex) {
    super(parent, MESSAGEML_TAG, DEFAULT_PRESENTATIONML_TAG, FormatEnum.MESSAGEML);
    this.entityId = getEntityId(entityIndex);
    this.size = DEFAULT_EMOJI_SIZE;
  }

  public String getAnnotation() {
    return this.annotation;
  }

  public String getFamily() {
    return family;
  }

  public String getSize() {
    return size;
  }

  @Override
  public void asPresentationML(XmlPrintStream out) {
    out.openElement(presentationMLTag, CLASS_ATTR, Entity.PRESENTATIONML_CLASS, ENTITY_ID_ATTR, entityId);

    for (Element child : getChildren()) {
      child.asPresentationML(out);
    }

    out.closeElement();
  }

  @Override
  public org.commonmark.node.Node asMarkdown() throws InvalidInputException {
    return new EmojiNode(annotation);
  }

  @Override
  public ObjectNode asEntityJson(ObjectNode parent) {
    JsonNode entityNode = parent.path(entityId);

    if (entityNode.isMissingNode()) {
      ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
      node.put(TYPE_FIELD, getEntityType());
      node.put(VERSION_FIELD, getEntityVersion());

      ObjectNode idNode = new ObjectNode(JsonNodeFactory.instance);
      idNode.put(ATTR_ANNOTATION, getAnnotation());
      idNode.put(ATTR_SIZE, getSize());
      idNode.put(UNICODE_FIELD, EmojiAnnotationToUnicode.getUnicode(annotation));

      if (getFamily() != null) {
        idNode.put(ATTR_FAMILY, getFamily());
      }

      node.set(DATA_FIELD, idNode);

      parent.set(entityId, node);
      return node;
    } else {
      //For preexisting data-entity-id the node type has already been validated by MessageMLParser
      return (ObjectNode) entityNode;
    }


  }



  @Override
  public void validate() throws InvalidInputException {
    if (this.annotation == null) {
      throw new InvalidInputException("The attribute \"annotation\" is required");
    }
  }

  @Override
  public String toString() {
    return "Emoji(" + getAnnotation() + ")";
  }

  @Override
  protected void buildAttribute(Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ATTR_ANNOTATION:
        this.annotation = getStringAttribute(item);
        break;
      case ATTR_FAMILY:
        this.family = getStringAttribute(item);
        break;
      case ATTR_SIZE:
        this.size = getStringAttribute(item);
        break;
      default:
        throw new InvalidInputException("Attribute \"" + item.getNodeName() + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }

  @Override
  protected String getEntityValue() {
    return annotation;
  }

  @Override
  protected String getEntitySubType() {
    return null;
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
