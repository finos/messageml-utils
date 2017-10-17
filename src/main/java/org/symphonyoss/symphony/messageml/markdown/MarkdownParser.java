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
import static org.symphonyoss.symphony.messageml.markdown.EntityDelimiterProcessor.GROUP_DELIMITER;
import static org.symphonyoss.symphony.messageml.markdown.EntityDelimiterProcessor.RECORD_DELIMITER;
import static org.symphonyoss.symphony.messageml.markdown.EntityDelimiterProcessor.TABLE;

import com.fasterxml.jackson.databind.JsonNode;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.commonmark.node.AbstractVisitor;
import org.commonmark.node.Block;
import org.commonmark.node.BlockQuote;
import org.commonmark.node.CustomBlock;
import org.commonmark.node.CustomNode;
import org.commonmark.node.Document;
import org.commonmark.node.Emphasis;
import org.commonmark.node.FencedCodeBlock;
import org.commonmark.node.HardLineBreak;
import org.commonmark.node.Heading;
import org.commonmark.node.HtmlInline;
import org.commonmark.node.IndentedCodeBlock;
import org.commonmark.node.ListBlock;
import org.commonmark.node.Node;
import org.commonmark.node.StrongEmphasis;
import org.commonmark.node.Text;
import org.commonmark.node.ThematicBreak;
import org.commonmark.parser.Parser;
import org.symphonyoss.symphony.messageml.elements.Bold;
import org.symphonyoss.symphony.messageml.elements.BulletList;
import org.symphonyoss.symphony.messageml.elements.CashTag;
import org.symphonyoss.symphony.messageml.elements.Element;
import org.symphonyoss.symphony.messageml.elements.Emoji;
import org.symphonyoss.symphony.messageml.elements.FormatEnum;
import org.symphonyoss.symphony.messageml.elements.HashTag;
import org.symphonyoss.symphony.messageml.elements.Italic;
import org.symphonyoss.symphony.messageml.elements.LineBreak;
import org.symphonyoss.symphony.messageml.elements.Link;
import org.symphonyoss.symphony.messageml.elements.ListItem;
import org.symphonyoss.symphony.messageml.elements.Mention;
import org.symphonyoss.symphony.messageml.elements.MessageML;
import org.symphonyoss.symphony.messageml.elements.OrderedList;
import org.symphonyoss.symphony.messageml.elements.Table;
import org.symphonyoss.symphony.messageml.elements.TableCell;
import org.symphonyoss.symphony.messageml.elements.TableRow;
import org.symphonyoss.symphony.messageml.elements.TextNode;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.EmojiNode;
import org.symphonyoss.symphony.messageml.markdown.nodes.KeywordNode;
import org.symphonyoss.symphony.messageml.markdown.nodes.MentionNode;
import org.symphonyoss.symphony.messageml.markdown.nodes.TableCellNode;
import org.symphonyoss.symphony.messageml.markdown.nodes.TableNode;
import org.symphonyoss.symphony.messageml.markdown.nodes.TableRowNode;
import org.symphonyoss.symphony.messageml.util.IDataProvider;

import java.util.HashSet;
import java.util.Set;
import java.util.TreeMap;

/**
 * Used for converting legacy messages in Markdown and JSON entities to MessageMLV2 documents.
 * @author lukasz
 * @since 3/30/17
 */
public class MarkdownParser extends AbstractVisitor {
  private static final Parser MARKDOWN_PARSER;
  private static final String INDEX = "index";
  private static final String INDEX_START = "indexStart";
  private static final String INDEX_END = "indexEnd";
  private static final String TYPE = "type";
  private static final String ID = "id";
  private static final String TEXT = "text";
  private final IDataProvider dataProvider;
  private MessageML messageML;
  private Element parent;
  private int index;

  static {
    Set<Class<? extends Block>> enabledBlockTypes = new HashSet<>();
    enabledBlockTypes.add(Heading.class);
    enabledBlockTypes.add(ThematicBreak.class);
    enabledBlockTypes.add(FencedCodeBlock.class);
    enabledBlockTypes.add(IndentedCodeBlock.class);
    enabledBlockTypes.add(BlockQuote.class);
    enabledBlockTypes.add(ListBlock.class);

    MARKDOWN_PARSER = Parser.builder()
        .enabledBlockTypes(enabledBlockTypes)
        .customDelimiterProcessor(new EntityDelimiterProcessor())
        .customDelimiterProcessor(new EmojiDelimiterProcessor())
        .build();
  }

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
    TextNode node = new TextNode(parent, text.getLiteral());
    parent.addChild(node);
    visitChildren(text);
  }

  @Override
  public void visit(HtmlInline tag) {
    TextNode node = new TextNode(parent, tag.getLiteral());
    parent.addChild(node);
    visitChildren(tag);
  }

  @Override
  public void visit(HardLineBreak hardLineBreak) {
    LineBreak node = new LineBreak(parent);
    parent.addChild(node);
    visitChildren(hardLineBreak);
  }

  @Override
  public void visit(org.commonmark.node.Paragraph paragraph) {
    if (!(parent instanceof ListItem)) {
      LineBreak node = new LineBreak(parent);
      parent.addChild(node);
    }
    visitChildren(paragraph);
  }

  @Override
  public void visit(Emphasis em) {
    Italic node = new Italic(parent);
    visitChildren(node, em);
  }

  @Override
  public void visit(StrongEmphasis b) {
    Bold node = new Bold(parent);
    visitChildren(node, b);
  }

  @Override
  public void visit(org.commonmark.node.Link a) {
    try {
      Link node = new Link(parent, a.getDestination(), dataProvider);
      node.validate();
      visitChildren(node, a);
    } catch (InvalidInputException e) {
      TextNode node = new TextNode(parent, a.getDestination());
      visitChildren(node, a);
    }
  }

  @Override
  public void visit(org.commonmark.node.BulletList ul) {
    BulletList node = new BulletList(parent);
    visitChildren(node, ul);
  }

  @Override
  public void visit(org.commonmark.node.OrderedList ol) {
    OrderedList node = new OrderedList(parent);
    visitChildren(node, ol);
  }

  @Override
  public void visit(org.commonmark.node.ListItem li) {
    ListItem node = new ListItem(parent);
    visitChildren(node, li);
  }

  @Override
  public void visit(CustomNode node) {
    if (node instanceof KeywordNode) {
      visit((KeywordNode) node);
    } else if (node instanceof MentionNode) {
      visit((MentionNode) node);
    } else if (node instanceof EmojiNode) {
      visit((EmojiNode) node);
    }
  }

  @Override
  public void visit(CustomBlock block) {
    if (block instanceof TableNode) {
      visit((TableNode) block);
    } else if (block instanceof TableRowNode) {
      visit((TableRowNode) block);
    } else if (block instanceof TableCellNode) {
      visit((TableCellNode) block);
    }
  }

  private void visit(EmojiNode emoji){
    Emoji node = new Emoji(parent, emoji.getAnnotation(),  ++index);
    parent.addChild(node);
  }

  private void visit(KeywordNode keyword) {
    switch (keyword.getPrefix()) {
      case HashTag.PREFIX:
        HashTag hashtag = new HashTag(parent, ++index, keyword.getText());
        visitChildren(hashtag, keyword);
        break;
      case CashTag.PREFIX:
        CashTag cashtag = new CashTag(parent, ++index, keyword.getText());
        visitChildren(cashtag, keyword);
        break;
    }
  }

  private void visit(MentionNode mention) {
    try {
      Mention node = new Mention(parent, ++index, mention.getUid(), dataProvider);
      node.validate();
      visitChildren(node, mention);
    } catch (InvalidInputException e) {
      String text = ObjectUtils.firstNonNull(mention.getPrettyName(), mention.getScreenName(), mention.getEmail(),
          String.valueOf(mention.getUid()));
      TextNode node = new TextNode(parent, text);
      visitChildren(node, mention);
    }
  }

  private void visit(TableNode table) {
    Table node = new Table(parent);
    visitChildren(node, table);
  }

  private void visit(TableRowNode tr) {
    TableRow node = new TableRow(parent);
    visitChildren(node, tr);
  }

  private void visit(TableCellNode td) {
    TableCell node = new TableCell(parent);
    visitChildren(node, td);
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
  private String enrichMarkdown(String message, JsonNode entitiesNode, JsonNode mediaNode) throws InvalidInputException {

    TreeMap<Integer, JsonNode> entities = new TreeMap<>();
    TreeMap<Integer, JsonNode> media = new TreeMap<>();

    if (entitiesNode != null) {
      validateEntities(entitiesNode);
      for (JsonNode node : entitiesNode.findParents(INDEX_START)) {
        entities.put(node.get(INDEX_START).intValue(), node);
      }
    }

    if (mediaNode != null) {
      validateMedia(mediaNode);
      for (JsonNode node : mediaNode.findParents(INDEX)) {
        media.put(node.get(INDEX).intValue(), node.get(TEXT));
      }
    }

    // If entity indices are outside the message, pad the message to the necessary length
    int lastIndex = Math.max((!entities.isEmpty()) ? entities.lastKey() : 0, (!media.isEmpty()) ? media.lastKey() : 0);
    if (message.length() <= lastIndex) {
      message = StringUtils.rightPad(message, lastIndex + 1);
    }

    StringBuilder output = new StringBuilder();

    for (int i = 0; i < message.length(); i++) {
      char c = message.charAt(i);

      if (entities.containsKey(i)) {
        JsonNode entity = entities.get(i);
        String entityType = entity.get(TYPE).textValue().toUpperCase();
        String id = entity.get(ID).textValue();

        output.append(ENTITY_DELIMITER);
        output.append(entityType).append(FIELD_DELIMITER);
        output.append(id);
        output.append(ENTITY_DELIMITER);

        // We explicitly check the entity indices above, but make double sure that we don't get into an infinite loop here
        int endIndex = entity.get(INDEX_END).intValue() - 1;
        i = Math.max(endIndex, i);

      } else if (media.containsKey(i)) {
        JsonNode table = media.get(i);

        output.append(ENTITY_DELIMITER);
        output.append(TABLE).append(FIELD_DELIMITER);
        for (JsonNode row : table) {
          for (JsonNode cell : row) {
            String text = cell.textValue();
            output.append(text).append(RECORD_DELIMITER);
          }
          output.append(GROUP_DELIMITER);
        }
        output.append(ENTITY_DELIMITER);
        output.append(c);
      } else {
        output.append(c);
      }
    }

    return output.toString();
  }

  /**
   * Verify that JSON entities contain the required fields and that entity indices are correct.
   */
  private void validateEntities(JsonNode data) throws InvalidInputException {
    for (JsonNode node : data.findParents(INDEX_START)) {

      for (String key : new String[] {INDEX_START, INDEX_END, ID, TYPE}) {
        if (node.path(key).isMissingNode()) {
          throw new InvalidInputException("Required field \"" + key + "\" missing from the entity payload");
        }
      }

      int startIndex = node.get(INDEX_START).intValue();
      int endIndex = node.get(INDEX_END).intValue();

      if (endIndex <= startIndex) {
        throw new InvalidInputException(String.format("Invalid entity payload: %s (start index: %s, end index: %s)",
            node.get(ID).textValue(), startIndex, endIndex));
      }

    }
  }

  /**
   * Verify that JSON media node contains valid table payload.
   */
  private void validateMedia(JsonNode data) throws InvalidInputException {
    for (JsonNode node : data.findParents(INDEX)) {
      JsonNode text = node.path(TEXT);
      if (!text.isMissingNode() && (!text.isArray() || !text.get(0).isArray())) {
        throw new InvalidInputException(String.format("Invalid table payload: %s (start index: %s, end index: %s)",
            text.asText(), node.get(INDEX)));
      }
    }
  }

  /**
   * Parse the Markdown message and entity JSON into a MessageML document.
   */
  public MessageML parse(String message, JsonNode entities, JsonNode media) throws InvalidInputException {
    this.index = 0;
    String enriched = enrichMarkdown(message, entities, media);
    Node markdown = MARKDOWN_PARSER.parse(enriched);
    markdown.accept(this);

    return messageML;
  }

}
