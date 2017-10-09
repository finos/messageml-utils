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

import org.commonmark.node.Link;
import org.commonmark.node.Node;
import org.commonmark.node.Text;
import org.commonmark.parser.delimiter.DelimiterProcessor;
import org.commonmark.parser.delimiter.DelimiterRun;
import org.symphonyoss.symphony.messageml.markdown.nodes.KeywordNode;
import org.symphonyoss.symphony.messageml.markdown.nodes.MentionNode;
import org.symphonyoss.symphony.messageml.markdown.nodes.TableCellNode;
import org.symphonyoss.symphony.messageml.markdown.nodes.TableNode;
import org.symphonyoss.symphony.messageml.markdown.nodes.TableRowNode;

/**
 * Custom processor for entities (tags, mentions, urls). Supports text processed by the method
 * <i>enrich()</i> in {@link MarkdownParser}.
 * @author lukasz
 * @since 4/11/17
 */
public class EntityDelimiterProcessor implements DelimiterProcessor {
  public static final char ENTITY_DELIMITER = '\u0091';
  public static final char FIELD_DELIMITER = '\u0092';
  public static final char GROUP_DELIMITER = '\u001D';
  public static final char RECORD_DELIMITER = '\u001E';
  public static final String KEYWORD = "KEYWORD";
  public static final String URL = "URL";
  public static final String MENTION = "USER_FOLLOW";
  public static final String TABLE = "TABLE";
  public static final String EMOJI = "EMOJI";

  @Override
  public char getOpeningCharacter() {
    return ENTITY_DELIMITER;
  }

  @Override
  public char getClosingCharacter() {
    return ENTITY_DELIMITER;
  }

  @Override
  public int getMinLength() {
    return 0;
  }

  @Override
  public int getDelimiterUse(DelimiterRun opener, DelimiterRun closer) {
    if (opener.length() >= 1 && closer.length() >= 1) {
      // Use exactly one delimiter even if we have more, and don't care about internal openers/closers.
      return 1;
    } else {
      return 0;
    }
  }

  private Node createNode(Text node) {
    String[] text = node.getLiteral().split(String.valueOf(FIELD_DELIMITER));
    if (text.length >= 2) {
      switch (text[0]) {
        case KEYWORD:
          String prefix = text[1].substring(0, 1);
          String value = text[1].substring(1);
          return new KeywordNode(prefix, value);

        case URL:
          return new Link(text[1], text[1]);

        case MENTION:
          try {
            Long uid = Long.valueOf(text[1]);
            return new MentionNode(uid);
          } catch (NumberFormatException e) {
            return new Text(text[1]);
          }

        case TABLE:
          TableNode tableNode = new TableNode();
          for (String row : text[1].split(String.valueOf(GROUP_DELIMITER))) {
            TableRowNode rowNode = new TableRowNode();
            for (String cell : row.split(String.valueOf(RECORD_DELIMITER))) {
              TableCellNode cellNode = new TableCellNode();
              cellNode.appendChild(new Text(cell));
              rowNode.appendChild(cellNode);
            }
            tableNode.appendChild(rowNode);
          }
          return tableNode;

        default:
          return null;
      }
    } else {
      return node;
    }
  }

  @Override
  public void process(Text opener, Text closer, int delimiterUse) {
    Node node = opener.getNext();

    if (node instanceof Text) {
      Node result = createNode((Text) node);

      if (result != null) {
        opener.insertAfter(result);
        node.unlink();
      }
    }
  }
}
