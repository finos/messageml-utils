/* ----------------------------------------------------------------------------
 * Copyright (C) 2016
 * Symphony Communication Services, LLC
 * All Rights Reserved
 * ---------------------------------------------------------------------------- */
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

package org.symphonyoss.symphony.messageml.elements;


import static org.symphonyoss.symphony.messageml.elements.UIAction.TARGET_ID;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.commonmark.node.Document;
import org.symphonyoss.symphony.messageml.MessageMLContext;
import org.symphonyoss.symphony.messageml.MessageMLParser;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.IDataProvider;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.symphonyoss.symphony.messageml.util.instrument.resolver.InstrumentKind;
import org.symphonyoss.symphony.messageml.util.instrument.resolver.InstrumentResolution;
import org.symphonyoss.symphony.messageml.util.instrument.resolver.MarketSector;
import org.symphonyoss.symphony.messageml.util.instrument.resolver.ResolutionResults;
import org.w3c.dom.Node;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;


/**
 * Class representing a MessageML document (i.e. a message).
 *
 * A MessageML document tree is constructed through one of parse() methods in
 * {@link org.symphonyoss.symphony.messageml.MessageMLContext}.
 * @author lukasz
 * @since 3/27/17
 */
public class MessageML extends Element {

  public static final String MESSAGEML_VERSION = "2.0";
  public static final String MESSAGEML_TAG = "messageML";
  public static final String MESSAGEML_XMLNS = "https://finos.org/messageml";
  public static final String PRESENTATIONML_TAG = "div";
  private static final String ATTR_FORMAT = "data-format";
  private static final String ATTR_VERSION = "data-version";
  private static final String ATTR_XMLNS = "xmlns";
  private static final String PRESENTATIONML_FORMAT = "PresentationML";

  private String version;
  private boolean chime;
  private String xmlns;

  public MessageML(FormatEnum format, String version) {
    super(null, MESSAGEML_TAG, format);
    this.version = version;
  }

  @Override
  protected void buildAttribute(MessageMLParser parser, Node item) throws InvalidInputException {
    if (getFormat() == FormatEnum.PRESENTATIONML) {
      switch (item.getNodeName()) {
        case ATTR_FORMAT:
          setAttribute(ATTR_FORMAT, getStringAttribute(item));
          break;

        case ATTR_VERSION:
          this.version = getStringAttribute(item);
          break;

        default:
          super.buildAttribute(parser, item);
      }
    } else {
      switch (item.getNodeName()) {
        case ATTR_XMLNS:
          this.xmlns = getStringAttribute(item);
          break;

        default:
          super.buildAttribute(parser, item);
      }
    }
  }

  @Override
  public Document asMarkdown() throws InvalidInputException {
    Document root = new Document();
    try {
      buildMarkdown(root);
    } catch (IllegalArgumentException e) {
      throw new InvalidInputException("Failed to build Markdown: " + e.getMessage());
    }
    return root;
  }

  @Override
  public void asPresentationML(XmlPrintStream out,
      MessageMLContext context) {

    out.openElement(PRESENTATIONML_TAG, ATTR_FORMAT, PRESENTATIONML_FORMAT, ATTR_VERSION, version);

    for (Element child : getChildren()) {
      child.asPresentationML(out, context);
    }

    out.closeElement();
  }

  @Override
  public ObjectNode asEntityJson(ObjectNode parent) {
    if (parent == null) {
      parent = new ObjectNode(JsonNodeFactory.instance);
    }

    buildEntityJson(parent);
    return parent;
  }

  @Override
  public void validate() throws InvalidInputException {
    if (format == FormatEnum.MESSAGEML) {

      assertNoAttributes();

    } else if (format == FormatEnum.PRESENTATIONML) {

      if (!PRESENTATIONML_FORMAT.equalsIgnoreCase(getAttribute(ATTR_FORMAT)) || this.version == null) {
        throw new InvalidInputException("Malformed PresentationML. The attributes \"" + ATTR_FORMAT
            + "\" and \"" + ATTR_VERSION + "\" are required.");
      }

    }

    if (isChime()) {

      if (getChildren().size() != 1 || !(getChild(0) instanceof Chime)) {
        throw new InvalidInputException("Chime messages may not have any other content");
      }

    }
    validateTargetIdForUIActions();
  }

  /**
   * Return whether this message is a chime.
   */

  public boolean isChime() {
    return chime;
  }

  /**
   * Return whether this message is a chime.
   */
  public void setChime(boolean chime) {
    this.chime = chime;
  }

  @Override
  public String getPresentationMLTag() {
    return PRESENTATIONML_TAG;
  }

  /**
   * If the messageML contains a uiAction with a target-id this method checks that exists a corresponding
   * dialog element with the same id.
   */
  private void validateTargetIdForUIActions() throws InvalidInputException {
    List<Element> uiActionWithTargetId =
        findElements(element -> element.getClass() == UIAction.class && element.getAttribute(TARGET_ID) != null);
    for(Element uiAction: uiActionWithTargetId) {
      Dialog dialog = findMatchingDialog((UIAction) uiAction);
      uiAction.setAttribute(TARGET_ID, dialog.getPresentationMlIdAttribute());
    }
  }

  public void enhanceFinancialTags(MessageML result, IDataProvider dataProvider)
      throws InvalidInputException {

    List<Tag> elements = result.getChildrenOfType(Tag.class)
        .stream()
        .map(element -> Tag.class.cast(element))
        .collect(Collectors.toList());
    if (elements != null && !elements.isEmpty()) {processFinancialTags(elements, dataProvider);}
  }

  private void processFinancialTags(List<Tag> elements, IDataProvider dataProvider)
      throws InvalidInputException {
    List<Pair<InstrumentResolution, Tag>> instrumentResolutionMap =
        IntStream.range(0, elements.size())
            .mapToObj(index -> buildInstrumentResolutionRequest(elements.get(index), index))
            .collect(Collectors.toList());
    // Build  resolver api request
    List<InstrumentResolution> criteria =
        instrumentResolutionMap.stream().map(Pair::getLeft).collect(Collectors.toList());
    ResolutionResults results = dataProvider.getFinTagPresentation(criteria);
    // update financial tag element data
    instrumentResolutionMap.forEach(entry -> {
          String resolutionId = entry.getLeft().getResolutionId();
          if (results != null && results.getInstruments() != null && results.getInstruments()
              .containsKey(resolutionId)) {
            entry.getRight()
                .setInstrument(
                    results.getInstruments().get(resolutionId).getInstrument());
          }
        }
    );
    for (Tag element : elements) {element.validateFallBackTicker();}
  }

  private Pair<InstrumentResolution, Tag> buildInstrumentResolutionRequest(Tag tag,
      Integer order) {
    InstrumentResolution resolution = new InstrumentResolution();
    resolution.setResolutionId(order.toString());
    resolution.setBbgCompTicker(tag.getTagAttributes().getBbgcompticker());
    resolution.setFigi(tag.getTagAttributes().getFigi());
    resolution.setFigiTicker(tag.getTagAttributes().getFigiTicker());
    resolution.setUniqueId(tag.getTagAttributes().getUniqueId());
    resolution.setIsin(tag.getTagAttributes().getIsin());
    resolution.setUsCode(tag.getTagAttributes().getUscode());
    resolution.setFullBbgCompTicker(tag.getTagAttributes().getFullBbgCompTicker());
    resolution.setLocalCode(tag.getTagAttributes().getLocalcode());
    resolution.setOperationalMic(tag.getTagAttributes().getOperationalMic());
    resolution.setInstrumentClass(
        InstrumentKind.fromValue(tag.getTagAttributes().getInstrumentclass()));
    resolution.setCountryCode(tag.getTagAttributes().getCountrycode());
    resolution.setReturnMainListing(tag.getTagAttributes().getReturnMainListing());
    resolution.setBbgMarketSector(
        MarketSector.fromValue(tag.getTagAttributes().getBbgmarketsector()));
    return new ImmutablePair<>(resolution, tag);
  }

}
