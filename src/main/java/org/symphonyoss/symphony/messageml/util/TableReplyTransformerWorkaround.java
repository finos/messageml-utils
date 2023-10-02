package org.symphonyoss.symphony.messageml.util;

import com.fasterxml.jackson.databind.JsonNode;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

/**
 * Workaround {@see <a href="https://perzoinc.atlassian.net/browse/PLAT-9005">PLAT-9005</a> }
 * The Symphony client send messages in PresentationML format; however, when replying to a
 * message, it uses markup.
 * In particular, when replying to a message containing a table, it add the following pattern:
 * _..._
 * The table is inserted just before the first dot; the underscore is to put the emphasis
 * attribute for the quoted message.
 * <p>
 * This is a problem here, when the agent tries to convert the markup message to PresentationML.
 * The markup language does not support an inline element (the emphasis) as a parent of a block
 * (the table).
 * This code tries to remove the emphasis around a table.
 */
public class TableReplyTransformerWorkaround {

  public static final String EXCEL_CONTENT_TYPE = "excel-rcp";
  private static final Pattern EMPHASIS = Pattern.compile("^_\\S.*\\S_$");

  private TableReplyTransformerWorkaround() {
    super();
  }


  /**
   * if the line containing the table start and end with the emphasis markup
   * it must be replaced with an empty space (this space will be removed during
   * PresentationML conversion.
   * it is not simply removed to avoid to recalculate all indexes in entities)
   * This code could be improved, instead of simply removing the emphasis, by rewriting it
   * around the
   * text -if any-
   * e.g. if you have the following line: _Hello [TABLE] world!_
   * if it could be rewritten as _Hello_ [TABLE] _world!_ (but all indexes in entities need to
   * be recalculated)
   *
   * @param markdown
   * @param media
   */
  public static void replaceEmphasis(StringBuilder markdown, JsonNode media) {

    Iterable<JsonNode> iterable = () -> media.path("content").elements();
    // extract tables from message, if any
    List<JsonNode> tables = StreamSupport.stream(iterable.spliterator(), false)
        .filter(node -> node.get("type").asText().equals(EXCEL_CONTENT_TYPE))
        .collect(Collectors.toList());
    // Process all tables to remove emphasis
    Set<Integer> processedLines = new HashSet<>();
    for (JsonNode table : tables) {
      int tableIndex = table.get("index").asInt();
      int startLineIndex = findStartLine(markdown, tableIndex);
      if (processedLines.contains(startLineIndex)) {
        // avoid to process multiple times the same line
        continue;
      }
      int endLineIndex = findEndLine(markdown, tableIndex);

      if (EMPHASIS.matcher(markdown.substring(startLineIndex, endLineIndex + 1)).matches()) {
        markdown.setCharAt(startLineIndex, ' ');
        markdown.setCharAt(endLineIndex, ' ');
      }
      processedLines.add(startLineIndex);
    }
  }

  private static int findStartLine(StringBuilder markdown, int index) {
    int newLine = markdown.substring(0, index).lastIndexOf('\n');
    return newLine < 0 ? 0 : newLine + 1;
  }

  private static int findEndLine(StringBuilder markdown, int index) {
    int newLine = markdown.indexOf("\n", index);
    return newLine < 0 ? markdown.length() - 1 : newLine - 1;
  }
}
