package org.symphonyoss.symphony.messageml.markdown;

import org.commonmark.node.Node;
import org.commonmark.node.Text;
import org.commonmark.parser.delimiter.DelimiterProcessor;
import org.commonmark.parser.delimiter.DelimiterRun;
import org.symphonyoss.symphony.messageml.elements.TextNode;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.markdown.nodes.EmojiNode;

public class EmojiDelimiterProcessor implements DelimiterProcessor {

  private final char delimiterChar;

  protected EmojiDelimiterProcessor() {
    this.delimiterChar = EmojiNode.DELIMITER;
  }

  @Override
  public char getOpeningCharacter() {
    return delimiterChar;
  }

  @Override
  public char getClosingCharacter() {
    return delimiterChar;
  }

  @Override
  public int getMinLength() {
    return 1;
  }

  @Override
  public int getDelimiterUse(DelimiterRun opener, DelimiterRun closer) {
    if (opener.length() >= 1 && closer.length() >= 1) {
      // Use exactly one delimiter even if we have more, and don't care about internal
      // openers/closers.
      return 1;
    } else {
      return 0;
    }
  }

  @Override
  public void process(Text opener, Text closer, int delimiterUse) {
    EmojiNode emoji = null;

    Node text = opener.getNext();
    if (text instanceof Text) {
      emoji = new EmojiNode(((Text) text).getLiteral());
      text.unlink();
    }

    opener.insertAfter(emoji);
  }
}