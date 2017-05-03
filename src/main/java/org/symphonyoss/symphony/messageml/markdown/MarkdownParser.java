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

package org.symphonyoss.symphony.messageml.markdown;

import static org.symphonyoss.symphony.messageml.markdown.EntityDelimiterProcessor.ENTITY_DELIMITER;
import static org.symphonyoss.symphony.messageml.markdown.EntityDelimiterProcessor.FIELD_DELIMITER;

import com.fasterxml.jackson.databind.JsonNode;
import org.apache.commons.lang3.ObjectUtils;
import org.commonmark.node.AbstractVisitor;
import org.commonmark.node.CustomNode;
import org.commonmark.node.Document;
import org.commonmark.node.Emphasis;
import org.commonmark.node.HardLineBreak;
import org.commonmark.node.Node;
import org.commonmark.node.StrongEmphasis;
import org.commonmark.node.Text;
import org.commonmark.parser.Parser;
import org.symphonyoss.symphony.messageml.elements.Bold;
import org.symphonyoss.symphony.messageml.elements.BulletList;
import org.symphonyoss.symphony.messageml.elements.CashTag;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.FormatEnum;
import org.symphonyoss.symphony.messageml.elements.HashTag;
import org.symphonyoss.symphony.messageml.elements.Italic;
import org.symphonyoss.symphony.messageml.elements.LineBreak;
import org.symphonyoss.symphony.messageml.elements.Link;
import org.symphonyoss.symphony.messageml.elements.ListItem;
import org.symphonyoss.symphony.messageml.elements.Mention;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.OrderedList;
import org.symphonyoss.symphony.messageml.elements.TextNode;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.KeywordNode;
import org.symphonyoss.symphony.messageml.markdown.nodes.MentionNode;
import org.symphonyoss.symphony.messageml.util.IDataProvider;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Used for converting legacy messages in Markdown and JSON entities to MessageMLV2 documents.
 * @author lukasz
 * @since 3/30/17
 */
public class MarkdownParser extends AbstractVisitor {
  private static final Parser MARKDOWN_PARSER = Parser.builder()
      .customDelimiterProcessor(new EntityDelimiterProcessor())
      .build();
  private final IDataProvider dataProvider;
  private MessageML messageML;
  private Element parent;
  private int index;
  private int size;

  public MarkdownParser(IDataProvider dataProvider) {
    this.dataProvider = dataProvider;
  }

  @Override
  public void visit(Document document) {
    messageML = new MessageML(FormatEnum.PRESENTATIONML, MessageML.MESSAGEML_VERSION);
    parent = messageML;
    visitChildren(document);
  }

  @Override
  public void visit(Text text) {
    this.index += text.getLiteral().length();

    TextNode node = new TextNode(parent, text.getLiteral());
    parent.addChild(node);
    visitChildren(text);
  }

  @Override
  public void visit(HardLineBreak hardLineBreak) {
    this.index += 1;

    LineBreak node = new LineBreak(++size, parent);
    parent.addChild(node);
    visitChildren(hardLineBreak);
  }

  @Override
  public void visit(org.commonmark.node.Paragraph paragraph) {
    this.index += 1;

    if (!(parent instanceof ListItem)) {
      LineBreak node = new LineBreak(++size, parent);
      parent.addChild(node);
    }
    visitChildren(paragraph);

    this.index += 1;
  }

  @Override
  public void visit(Emphasis em) {
    this.index += em.getOpeningDelimiter().length();

    Italic node = new Italic(++size, parent);
    visitChildren(node, em);

    this.index += em.getClosingDelimiter().length();
  }

  @Override
  public void visit(StrongEmphasis b) {
    this.index += b.getOpeningDelimiter().length();

    Bold node = new Bold(++size, parent);
    visitChildren(node, b);

    this.index += b.getClosingDelimiter().length();
  }

  @Override
  public void visit(org.commonmark.node.Link a) {
    try {
      Link node = new Link(++size, parent, a.getDestination(), dataProvider);
      node.validate();
      visitChildren(node, a);
    } catch (InvalidInputException e) {
      TextNode node = new TextNode(parent, a.getDestination());
      visitChildren(node, a);
    }
  }

  @Override
  public void visit(org.commonmark.node.BulletList ul) {
    BulletList node = new BulletList(++size, parent);
    visitChildren(node, ul);
  }

  @Override
  public void visit(org.commonmark.node.OrderedList ol) {
    OrderedList node = new OrderedList(++size, parent);
    visitChildren(node, ol);
  }

  @Override
  public void visit(org.commonmark.node.ListItem li) {
    ListItem node = new ListItem(++size, parent);
    visitChildren(node, li);
  }

  @Override
  public void visit(CustomNode node) {
    if (node instanceof KeywordNode) {
      visit((KeywordNode) node);
    } else if (node instanceof MentionNode) {
      visit((MentionNode) node);
    }
  }

  private void visit(KeywordNode keyword) {
    switch (keyword.getPrefix()) {
      case HashTag.PREFIX:
        HashTag hashtag = new HashTag(++size, parent, keyword.getText(), FormatEnum.MESSAGEML);
        visitChildren(hashtag, keyword);
        break;
      case CashTag.PREFIX:
        CashTag cashtag = new CashTag(++size, parent, keyword.getText(), FormatEnum.MESSAGEML);
        visitChildren(cashtag, keyword);
        break;
    }
  }

  private void visit(MentionNode mention) {
    try {
      Mention node = new Mention(++size, parent, mention.getUid(), true, dataProvider, FormatEnum.MESSAGEML);
      node.validate();
      visitChildren(node, mention);
    } catch (InvalidInputException e) {
      String text = ObjectUtils.firstNonNull(mention.getPrettyName(), mention.getScreenName(), mention.getEmail(),
          String.valueOf(mention.getUid()));
      TextNode node = new TextNode(parent, text);
      visitChildren(node, mention);
    }
  }

  private void visitChildren(Element element, Node node) {
    parent.addChild(element);
    parent = element;
    visitChildren(node);
    parent = element.getParent();
  }

  /**
   * Generate intermediate markup to delimit custom nodes representing entities for further processing by the
   * Markdown parser.
   */
  private String enrichMarkdown(String message, JsonNode data) {
    Map<Integer, JsonNode> entities = new LinkedHashMap<>();
    for (JsonNode node : data.findParents("indexStart")) {
      entities.put(node.get("indexStart").intValue(), node);
    }

    StringBuilder output = new StringBuilder();

    for (int i = 0; i < message.length(); i++) {
      char c = message.charAt(i);

      if (entities.containsKey(i)) {
        JsonNode entity = entities.get(i);
        String entityType = entity.get("type").textValue().toUpperCase();
        String id = entity.get("id").textValue();

        output.append(ENTITY_DELIMITER);
        output.append(entityType).append(FIELD_DELIMITER);
        output.append(id);
        output.append(ENTITY_DELIMITER);

        i = entity.get("indexEnd").intValue() - 1;
      } else {
        output.append(c);
      }
    }

    return output.toString();
  }

  /**
   * Parse the Markdown message and entity JSON into a MessageML document.
   */
  public MessageML parse(String message, JsonNode data) {
    String enriched = enrichMarkdown(message, data);
    Node markdown = MARKDOWN_PARSER.parse(enriched);
    markdown.accept(this);

    return messageML;
  }


}
