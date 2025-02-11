/*
 * Copyright 2016-2017 MessageML - Symphony LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.finos.symphony.messageml.messagemlutils.elements;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.commonmark.node.Node;
import org.finos.symphony.messageml.messagemlutils.MessageMLContext;
import org.finos.symphony.messageml.messagemlutils.MessageMLParser;
import org.finos.symphony.messageml.messagemlutils.bi.BiContext;
import org.finos.symphony.messageml.messagemlutils.bi.BiFields;
import org.finos.symphony.messageml.messagemlutils.bi.BiItem;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.markdown.nodes.TagNode;
import org.finos.symphony.messageml.messagemlutils.util.TagAttributes;
import org.finos.symphony.messageml.messagemlutils.util.XmlPrintStream;
import org.finos.symphony.messageml.messagemlutils.util.instrument.resolver.Instrument;
import org.finos.symphony.messageml.messagemlutils.util.instrument.resolver.InstrumentKind;
import org.finos.symphony.messageml.messagemlutils.util.instrument.resolver.MarketSector;

import java.util.Collections;

/**
 * Class representing a convenience element for a financial tag .
 */
public class Tag extends Entity {
  public static final String MESSAGEML_TAG = "tag";
  public static final String PREFIX = "$";
  public static final String ENTITY_TYPE = "org.symphonyoss.fin.security";
  private static final String ENTITY_SUBTYPE = "org.symphonyoss.fin.security.id.ticker";
  private static final String ENTITY_VERSION = "2.0";
  private static final String LEGACY_ENTITY_VERSION = "1.0";
  @Getter
  private TagAttributes tagAttributes = new TagAttributes();
  @Setter
  private Instrument instrument;


  public Tag(Element parent, int entityIndex) {
    this(parent, DEFAULT_PRESENTATIONML_TAG, entityIndex, FormatEnum.MESSAGEML);
  }

  private Tag(Element parent, String presentationMlTag, Integer entityIndex, FormatEnum format) {
    super(parent, MESSAGEML_TAG, presentationMlTag, format);
    this.entityId = getEntityId(entityIndex);
  }

  @Override
  protected void buildAttribute(MessageMLParser parser,
      org.w3c.dom.Node item) throws InvalidInputException {
    switch (item.getNodeName()) {
      case TagAttributes.ATTR_FULLBBGCOMPTICKER:
        tagAttributes.setFullBbgCompTicker(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_UNIQUEID:
        tagAttributes.setUniqueId(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_FIGI:
        tagAttributes.setFigi(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_BBGCOMPTICKER:
        tagAttributes.setBbgcompticker(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_FIGITICKER:
        tagAttributes.setFigiTicker(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_USCODE:
        tagAttributes.setUscode(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_ISIN:
        tagAttributes.setIsin(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_LOCALCODE:
        tagAttributes.setLocalcode(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_INSTRUMENTCLASS:
        tagAttributes.setInstrumentclass(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_BBGMARKETSECTOR:
        tagAttributes.setBbgmarketsector(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_RETURNMAINLISTING:
        tagAttributes.setReturnMainListing(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_COUNTRYCODE:
        tagAttributes.setCountrycode(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_OPERATIONALMIC:
        tagAttributes.setOperationalMic(getStringAttribute(item));
        break;
      case TagAttributes.ATTR_FALLBACKTICKER:
        tagAttributes.setFallbackTicker(getStringAttribute(item));
        break;
      default:
        super.buildAttribute(parser, item);
    }
  }

  @Override
  public void validate() throws InvalidInputException {
    super.validate();
    if (this.tagAttributes.getInstrumentclass() != null
        && InstrumentKind.fromValue(this.tagAttributes.getInstrumentclass()) == null) {
      throw new InvalidInputException(
          "the attribute \"instrument-class\" must be one of those values "
              + InstrumentKind.toValues());
    }
    if (this.tagAttributes.getBbgmarketsector() != null
        && MarketSector.fromValue(this.tagAttributes.getBbgmarketsector()) == null) {
      throw new InvalidInputException(
          "the attribute \"bbgmarket-sector\" must be one of those values "
              + MarketSector.toValues());
    }
  }

  @Override
  public void asPresentationML(XmlPrintStream out, MessageMLContext context) {
    out.printElement(presentationMLTag, asText(), CLASS_ATTR, PRESENTATIONML_CLASS,
        ENTITY_ID_ATTR, entityId);
  }

  @Override
  Node asMarkdown() throws InvalidInputException {
    String text =
        instrument == null ? tagAttributes.getFallbackTicker() : instrument.getRootBbgCompTicker();
    JsonNode data =
        instrument != null ? MAPPER.convertValue(instrument, JsonNode.class) : null;
    return new TagNode(PREFIX, text, data);
  }

  @Override
  public ObjectNode asEntityJson(ObjectNode parent) {
    ObjectNode node = super.asEntityJson(parent);
    if (instrument != null) {
      enhanceInstrumentEntityJson(node);
    }
    return node;
  }


  private void enhanceInstrumentEntityJson(ObjectNode node) {
    ArrayNode idArray = ArrayNode.class.cast(node.get(ID_FIELD));
    idArray.add(
        buildNode("org.symphonyoss.fin.security.id.uniqueId", instrument.getUniqueId()));
    idArray.add(buildNode("org.symphonyoss.fin.security.id.fullBbgTicker",
        instrument.getFullBbgCompTicker()));
    idArray.add(buildNode("org.symphonyoss.fin.security.bbgcompticker",
        instrument.getBbgCompTicker()));
    idArray.add(buildNode("org.symphonyoss.fin.security.id.isin", instrument.getIsin()));
    idArray.add(buildNode("org.symphonyoss.fin.security.id.figi", instrument.getFigi()));
    idArray.add(buildNode("org.symphonyoss.fin.security.id.figiTicker",
        instrument.getFigiTicker()));
    idArray.add(
        buildNode("org.symphonyoss.fin.security.id.lei", instrument.getFigiTicker()));
    idArray.add(
        buildNode("org.symphonyoss.fin.security.id.localCode", instrument.getLocalCode()));
    idArray.add(buildNode("org.symphonyoss.fin.security.id.operationalMic",
        instrument.getOperationalMic()));
    idArray.add(buildNode("org.symphonyoss.fin.security.countryCode", instrument.getCountryCode()));
    idArray.add(buildNode("org.symphonyoss.fin.security.countryName", instrument.getCountryName()));
    idArray.add(
        buildNode("org.symphonyoss.fin.security.exchangeName", instrument.getExchangeName()));
    idArray.add(buildNode("org.symphonyoss.fin.security.displayName", instrument.getDisplayName()));
    idArray.add(buildNode("org.symphonyoss.fin.security.currency", instrument.getCurrency()));
    idArray.add(buildNode("org.symphonyoss.fin.security.instrumentTypeCode",
        instrument.getInstrumentTypeCode()));
    idArray.add(buildNode("org.symphonyoss.fin.security.instrumentTypeName",
        instrument.getInstrumentTypeName()));
  }

  private ObjectNode buildNode(String type, String value) {
    ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
    node.put(TYPE_FIELD, type);
    node.put(VALUE_FIELD, value);
    return node;
  }

  @Override
  protected String getEntityIdPrefix() {
    return MESSAGEML_TAG;
  }

  @Override
  protected String getEntityValue() {
    return instrument == null ? tagAttributes.getFallbackTicker()
        : instrument.getRootBbgCompTicker();
  }

  @Override
  protected String getEntitySubType() {
    return ENTITY_SUBTYPE;
  }

  @Override
  protected String getEntityVersion() {
    return instrument == null ? LEGACY_ENTITY_VERSION : ENTITY_VERSION;
  }

  @Override
  protected String getEntityType() {
    return ENTITY_TYPE;
  }

  @Override
  public String asText() {
    String text = instrument == null ? tagAttributes.getFallbackTicker()
        : instrument.getRootBbgCompTicker();
    return PREFIX + text;
  }

  public void validateFallBackTicker() throws InvalidInputException {
    if (instrument == null && StringUtils.isBlank(tagAttributes.getFallbackTicker())) {
      throw new InvalidInputException(
          "No instrument found , \"fallback-ticker\" attribute is required");
    }
  }

  @Override
  public void updateBiContext(BiContext context) {
    super.updateBiContext(context);
    context.updateItemCount(BiFields.HASHTAGS.getValue());
    context.addItem(new BiItem(BiFields.ENTITY.getValue(),
        Collections.singletonMap(BiFields.ENTITY_TYPE.getValue(), this.getEntityType())));
  }
}
