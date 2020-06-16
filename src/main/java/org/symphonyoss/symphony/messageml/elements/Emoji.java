package org.symphonyoss.symphony.messageml.elements;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.EmojiNode;
import org.symphonyoss.symphony.messageml.util.EmojiShortcodeToUnicode;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Node;


/**
 * Class representing an emoji
 * @author cristiadu
 * @since 10/10/17
 */

public class Emoji extends Entity {

  public static final String MESSAGEML_TAG = "emoji";
  @Deprecated
  private static final String ATTR_ANNOTATION = "annotation";
  private static final String ATTR_SHORTCODE = "shortcode";
  private static final String SHORTCODE_PATTERN = "[\\p{Alnum}_+-]*";
  private static final String ATTR_FAMILY = "family";
  private static final String ATTR_SIZE = "size";

  private static final String ENTITY_TYPE = "com.symphony.emoji";
  private static final String ENTITY_VERSION = "1.0";
  private static final String ENTITY_ID_PREFIX = "emoji";
  private static final String DELIMITER = ":";
  private static final String DATA_FIELD = "data";
  private static final String UNICODE_FIELD = "unicode";
  private static final String DEFAULT_EMOJI_SIZE = "normal";

  @Deprecated
  private String annotation;
  private String shortcode;
  private String family;
  private String size;

  public Emoji(Element parent, String shortcode, int entityIndex) {
    this(parent, entityIndex);
    this.annotation = shortcode;
    this.shortcode = shortcode;
  }

  public Emoji(Element parent, int entityIndex) {
    super(parent, MESSAGEML_TAG, DEFAULT_PRESENTATIONML_TAG, FormatEnum.MESSAGEML);
    this.entityId = getEntityId(entityIndex);
    this.size = DEFAULT_EMOJI_SIZE;
  }

  /**
   * It is deprecated, use getShortcode() instead
   * @return annotation
   */
  public String getAnnotation() {
    return this.annotation;
  }

  public String getShortCode() {
    return this.shortcode;
  }

  public String getFamily() {
    return family;
  }

  public String getSize() {
    return size;
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {
    out.openElement(presentationMLTag, CLASS_ATTR, Entity.PRESENTATIONML_CLASS, ENTITY_ID_ATTR, entityId);

    if (this.getChildren().isEmpty()) {
      out.append(asDefaultRepresentation());
    } else {
      for (Element child : getChildren()) {
        child.asPresentationML(out, context);
      }
    }

    out.closeElement();
  }

  @Override
  public org.commonmark.node.Node asMarkdown() throws InvalidInputException {
    return new EmojiNode(shortcode);
  }

  @Override
  public String asText() {
    StringBuilder b = new StringBuilder();

    if (this.getChildren().isEmpty()) {
      b.append(asDefaultRepresentation());
    } else {
      for (Element child : this.getChildren()) {
        b.append(child.asText());
      }
    }

    return b.toString();
  }

  @Override
  public ObjectNode asEntityJson(ObjectNode parent) {
    JsonNode entityNode = parent.path(entityId);

    if (entityNode.isMissingNode()) {
      ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
      node.put(TYPE_FIELD, getEntityType());
      node.put(VERSION_FIELD, getEntityVersion());

      ObjectNode idNode = new ObjectNode(JsonNodeFactory.instance);
      idNode.put(ATTR_SHORTCODE, getShortCode());
      idNode.put(ATTR_ANNOTATION, getAnnotation());
      idNode.put(ATTR_SIZE, getSize());

      if (EmojiShortcodeToUnicode.hasUnicodeRepresentation(shortcode)) {
        idNode.put(UNICODE_FIELD, EmojiShortcodeToUnicode.getUnicode(shortcode));
      }

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
    if (this.shortcode == null) {
      throw new InvalidInputException("Either the attribute \"shortcode\" or \"annotation\" are required");
    } else if (!this.shortcode.matches(SHORTCODE_PATTERN)) {
      throw new InvalidInputException("Shortcode or Annotation parameter may only contain alphanumeric characters, underscore, plus sign and dash");
    }

    assertPhrasingContent();
  }

  @Override
  public String toString() {
    return "Emoji(" + getShortCode() + ")";
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ATTR_SHORTCODE:
      case ATTR_ANNOTATION:
        this.shortcode = getStringAttribute(item);
        this.annotation = this.shortcode;
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
    return shortcode;
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

  private String asDefaultRepresentation() {
    if (EmojiShortcodeToUnicode.hasUnicodeRepresentation(shortcode)) {
      return EmojiShortcodeToUnicode.getUnicode(shortcode);
    } else {
      return DELIMITER + shortcode + DELIMITER;
    }
  }

}
