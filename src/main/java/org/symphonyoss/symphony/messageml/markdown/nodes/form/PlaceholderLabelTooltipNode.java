package org.symphonyoss.symphony.messageml.markdown.nodes.form;

public interface PlaceholderLabelTooltipNode {
  default String generateMarkdownPlaceholderLabelAndTooltip(String placeholder, String label, String tooltip) {
    StringBuilder markdownRepresentation = new StringBuilder();
    
    if(placeholder != null || label != null || tooltip != null) {
      markdownRepresentation.append(":");
    }

    if(placeholder != null) {
      markdownRepresentation.append("[")
          .append(placeholder)
          .append("]");
    }
    
    if(label != null) {
      markdownRepresentation.append("[")
          .append(label)
          .append("]");
    }

    if(tooltip != null) {
      markdownRepresentation.append("[")
          .append(tooltip)
          .append("]");
    }
    
    return markdownRepresentation.toString();
  }
}
