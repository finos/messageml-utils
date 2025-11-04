package org.finos.symphony.messageml.messagemlutils.elements;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.commonmark.node.Node;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.bi.BiContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.markdown.nodes.DateTimeNode;
import org.finos.symphony.messageml.messagemlutils.util.XmlPrintStream;

import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * MessageML entity that allows to display a time following local time zone and user preference
 * Fields:
 *  * value : time value according to ISO 8601 with date, time and timezone
 *      * example: 2024-12-31T09:29:47Z
 *   * format: which format to use to display the date
 *      * available values: date, time, date_time, time_secs
 */
public class DateTime extends Entity {
  public static final String MESSAGEML_TAG = "dateTime";
  private static final String PRESENTATIONML_TAG = "time";

  private static final String ENTITY_TYPE = "org.symphonyoss";
  private static final String ENTITY_SUBTYPE = "org.symphonyoss.datetime";
  private static final String ENTITY_VERSION = "1.0";
  private static final String ENTITY_ID_PREFIX = "datetime";

  private static final String ATTR_VALUE = "value";
  private static final String ATTR_FORMAT = "format";

  private static final String ATTR_PML_FORMAT = "data-format";
  private static final String ATTR_PML_VALUE = "datetime";

  private static final String DATA_NODE = "data";

  private static List<String> VALID_DATE_FORMATS = Arrays.asList("date_time", "date", "time", "time_secs");

  private String value;
  private String dateFormat;


  public DateTime(Element parent, int entityIndex) {
    super(parent, MESSAGEML_TAG, PRESENTATIONML_TAG, FormatEnum.MESSAGEML);
    this.entityId = getEntityId(entityIndex);
  }

  @Override
  protected String getEntityValue() {
    return value;
  }

  @Override
  protected String getEntitySubType() {
    return ENTITY_SUBTYPE;
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

  @Override
  public void validate() throws InvalidInputException {
    super.validate();

    assertAttributeNotBlank(ATTR_VALUE);
    assertDateFormat(ATTR_VALUE, DateTimeFormatter.ISO_OFFSET_DATE_TIME);

    if (getAttribute(ATTR_FORMAT) != null) {
      assertAttributeValue(ATTR_FORMAT, VALID_DATE_FORMATS);
    }

  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case ATTR_VALUE:
        this.value = getStringAttribute(item);
        setAttribute(ATTR_VALUE, this.value);
        break;
      case ATTR_FORMAT:
        this.dateFormat = getStringAttribute(item);
        setAttribute(ATTR_FORMAT, dateFormat);
        break;
      default:
        throw new InvalidInputException( "Attribute \"" + item.getNodeName() + "\" is not allowed in \"" + getMessageMLTag() + "\"");
    }
  }


  @Override
  public void asPresentationML(XmlPrintStream out, MessageMLContext context) {
    Map<String, String> attributes = new HashMap<>();
    attributes.put(ENTITY_ID_ATTR, entityId);

    if (getAttribute(ATTR_FORMAT) != null) {
      attributes.put(ATTR_PML_FORMAT, getAttribute(ATTR_FORMAT));
    }
    attributes.put(ATTR_PML_VALUE, getAttribute(ATTR_VALUE));

    out.openElement(DEFAULT_PRESENTATIONML_TAG);
    out.printElement(PRESENTATIONML_TAG, value, attributes);
    out.closeElement();
  }

  @Override
  Node asMarkdown() throws InvalidInputException {
    return new DateTimeNode(entityId, value, dateFormat);
  }

  @Override
  public ObjectNode asEntityJson(ObjectNode parent) {
      JsonNode entityNode = parent.path(entityId);

      if (entityNode.isMissingNode()) {
        ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
        node.put(Entity.TYPE_FIELD, getEntityType());
        node.put(Entity.VERSION_FIELD, getEntityVersion());

        ObjectNode dataNode = new ObjectNode(JsonNodeFactory.instance);
        dataNode.put(Entity.VALUE_FIELD, value);
        dataNode.put(Entity.TYPE_FIELD, getEntitySubType());
        dataNode.put(ATTR_FORMAT, dateFormat);

        node.set(DATA_NODE, dataNode);

        parent.set(entityId, node);

        return node;

      } else {
        //For preexisting data-entity-id the node type has already been validated by MessageMLParser
        return (ObjectNode) entityNode;
      }
  }

  @Override
  public void updateBiContext(BiContext context) {
    super.updateBiContext(context);
    context.updateItemCount(BiFields.DATE_TIME.getValue());
    context.addItem(new BiItem(BiFields.ENTITY.getValue(),
        Collections.singletonMap(BiFields.ENTITY_TYPE.getValue(), this.getEntitySubType())));
  }
}
