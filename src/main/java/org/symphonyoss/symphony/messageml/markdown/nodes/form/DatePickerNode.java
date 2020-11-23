package org.symphonyoss.symphony.messageml.markdown.nodes.form;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class DatePickerNode extends FormElementNode {
  private final static String MARKDOWN = "Date Picker";

  private String label;
  private String tooltip;
  private String placeholder;

  public DatePickerNode(String label, String tooltip, String placeholder) {
    super(MARKDOWN, placeholder);
    this.label = label;
    this.tooltip = tooltip;
    this.placeholder = placeholder;
  }

  @Override
  public String getText() {
    List<String> text = Arrays.asList(label, tooltip, placeholder)
        .stream().filter(e -> e != null && !e.isEmpty()).collect(Collectors.toList());
    if(text.isEmpty()){
      return "";
    }
    StringBuilder result = new StringBuilder(":");
    text.forEach(word -> result.append("[").append(word).append("]"));
    return result.toString();
  }
}
