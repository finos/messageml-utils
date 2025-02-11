package org.finos.symphony.messageml.messagemlutils.markdown.nodes;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.commonmark.node.CustomNode;

@Getter
@AllArgsConstructor
public class DateTimeNode extends CustomNode {
  private String entityId;
  private String value;
  private String format;
}
