package org.symphonyoss.symphony.messageml.elements;

import org.apache.commons.lang3.StringUtils;
import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Static set of styles for the style Global Attribute
 */
public class Styles {

  public static Set<String> ALLOWED_PROPERTIES = new HashSet<>();
  static {
    ALLOWED_PROPERTIES.add("background");
    ALLOWED_PROPERTIES.add("background-attachment");
    ALLOWED_PROPERTIES.add("background-blend-mode");
    ALLOWED_PROPERTIES.add("background-clip");
    ALLOWED_PROPERTIES.add("background-color");
    ALLOWED_PROPERTIES.add("background-image");
    ALLOWED_PROPERTIES.add("background-position");
    ALLOWED_PROPERTIES.add("background-repeat");
    ALLOWED_PROPERTIES.add("background-size");
    ALLOWED_PROPERTIES.add("border");
    ALLOWED_PROPERTIES.add("border-bottom");
    ALLOWED_PROPERTIES.add("border-bottom-color");
    ALLOWED_PROPERTIES.add("border-bottom-left-radius");
    ALLOWED_PROPERTIES.add("border-bottom-right-radius");
    ALLOWED_PROPERTIES.add("border-bottom-style");
    ALLOWED_PROPERTIES.add("border-bottom-width");
    ALLOWED_PROPERTIES.add("border-collapse");
    ALLOWED_PROPERTIES.add("border-color");
    ALLOWED_PROPERTIES.add("border-image");
    ALLOWED_PROPERTIES.add("border-image-outset");
    ALLOWED_PROPERTIES.add("border-image-repeat");
    ALLOWED_PROPERTIES.add("border-image-slice");
    ALLOWED_PROPERTIES.add("border-image-source");
    ALLOWED_PROPERTIES.add("border-image-width");
    ALLOWED_PROPERTIES.add("border-left");
    ALLOWED_PROPERTIES.add("border-left-color");
    ALLOWED_PROPERTIES.add("border-left-style");
    ALLOWED_PROPERTIES.add("border-left-width");
    ALLOWED_PROPERTIES.add("border-radius");
    ALLOWED_PROPERTIES.add("border-right");
    ALLOWED_PROPERTIES.add("border-right-color");
    ALLOWED_PROPERTIES.add("border-right-style");
    ALLOWED_PROPERTIES.add("border-right-width");
    ALLOWED_PROPERTIES.add("border-spacing");
    ALLOWED_PROPERTIES.add("border-style");
    ALLOWED_PROPERTIES.add("border-top");
    ALLOWED_PROPERTIES.add("border-top-color");
    ALLOWED_PROPERTIES.add("border-top-left-radius");
    ALLOWED_PROPERTIES.add("border-top-right-radius");
    ALLOWED_PROPERTIES.add("border-top-style");
    ALLOWED_PROPERTIES.add("border-top-width");
    ALLOWED_PROPERTIES.add("border-width");
    ALLOWED_PROPERTIES.add("box-shadow");
    ALLOWED_PROPERTIES.add("box-sizing");
    ALLOWED_PROPERTIES.add("caption-side");
    ALLOWED_PROPERTIES.add("clear");
    ALLOWED_PROPERTIES.add("color");
    ALLOWED_PROPERTIES.add("content");
    ALLOWED_PROPERTIES.add("counter-increment");
    ALLOWED_PROPERTIES.add("counter-reset");
    ALLOWED_PROPERTIES.add("display");
    ALLOWED_PROPERTIES.add("empty-cells");
    ALLOWED_PROPERTIES.add("font");
    ALLOWED_PROPERTIES.add("font-family");
    ALLOWED_PROPERTIES.add("font-kerning");
    ALLOWED_PROPERTIES.add("font-size");
    ALLOWED_PROPERTIES.add("font-size-adjust");
    ALLOWED_PROPERTIES.add("font-stretch");
    ALLOWED_PROPERTIES.add("font-style");
    ALLOWED_PROPERTIES.add("font-variant");
    ALLOWED_PROPERTIES.add("font-weight");
    ALLOWED_PROPERTIES.add("height");
    ALLOWED_PROPERTIES.add("letter-spacing");
    ALLOWED_PROPERTIES.add("line-height");
    ALLOWED_PROPERTIES.add("list-style");
    ALLOWED_PROPERTIES.add("list-style-image");
    ALLOWED_PROPERTIES.add("list-style-position");
    ALLOWED_PROPERTIES.add("list-style-type");
    ALLOWED_PROPERTIES.add("margin");
    ALLOWED_PROPERTIES.add("margin-bottom");
    ALLOWED_PROPERTIES.add("margin-left");
    ALLOWED_PROPERTIES.add("margin-right");
    ALLOWED_PROPERTIES.add("margin-top");
    ALLOWED_PROPERTIES.add("max-height");
    ALLOWED_PROPERTIES.add("max-width");
    ALLOWED_PROPERTIES.add("min-height");
    ALLOWED_PROPERTIES.add("min-width");
    ALLOWED_PROPERTIES.add("opacity");
    ALLOWED_PROPERTIES.add("outline");
    ALLOWED_PROPERTIES.add("outline-color");
    ALLOWED_PROPERTIES.add("outline-offset");
    ALLOWED_PROPERTIES.add("outline-style");
    ALLOWED_PROPERTIES.add("outline-width");
    ALLOWED_PROPERTIES.add("overflow");
    ALLOWED_PROPERTIES.add("overflow-x");
    ALLOWED_PROPERTIES.add("overflow-y");
    ALLOWED_PROPERTIES.add("padding");
    ALLOWED_PROPERTIES.add("padding-bottom");
    ALLOWED_PROPERTIES.add("padding-left");
    ALLOWED_PROPERTIES.add("padding-right");
    ALLOWED_PROPERTIES.add("padding-top");
    ALLOWED_PROPERTIES.add("table-layout");
    ALLOWED_PROPERTIES.add("text-align");
    ALLOWED_PROPERTIES.add("text-align-last");
    ALLOWED_PROPERTIES.add("text-decoration");
    ALLOWED_PROPERTIES.add("text-decoration-color");
    ALLOWED_PROPERTIES.add("text-decoration-line");
    ALLOWED_PROPERTIES.add("text-decoration-style");
    ALLOWED_PROPERTIES.add("text-indent");
    ALLOWED_PROPERTIES.add("text-justify");
    ALLOWED_PROPERTIES.add("text-overflow");
    ALLOWED_PROPERTIES.add("text-shadow");
    ALLOWED_PROPERTIES.add("text-transform");
    ALLOWED_PROPERTIES.add("visibility");
    ALLOWED_PROPERTIES.add("white-space");
    ALLOWED_PROPERTIES.add("width");
    ALLOWED_PROPERTIES.add("word-break");
    ALLOWED_PROPERTIES.add("word-spacing");
    ALLOWED_PROPERTIES.add("word-wrap");
  }

  /**
   * Validate that the input style attribute is allowed
   *
   * @param styleAttribute input style string
   * @throws InvalidInputException if the styleAttribute is allowed
   */
  public static void validate(String styleAttribute) throws InvalidInputException {
    try {
      /*
       * The split of style properties does not use simply ';' as separator but a regular expression
       * to avoid splitting when there is a data URI scheme (data:[<media type>][;base64],<data>),
       * because it contains inside ';' and it must be not split
       *
       * This is a workaround, because for now it is the only exception found.
       * If in the future other similar situation will occurs, please consider if a CSS parser library can be used
       */
      final Set<String> inputStyleProperties = Arrays.asList(styleAttribute.split(";(?!base64,)")).stream()
          .filter(input -> !StringUtils.isBlank(input))
          .map(input -> {
              int separator = input.indexOf(":");
              if(separator == -1){
                throw new IllegalArgumentException(String.format("Chunk [%s] is not a valid entry", input));
              }
              return input.substring(0, separator).trim();
            }
          )
          .collect(Collectors.toSet());
      inputStyleProperties.removeAll(ALLOWED_PROPERTIES);
      if (!inputStyleProperties.isEmpty()) {
        throw new InvalidInputException("Invalid property(s): [" + StringUtils.join(inputStyleProperties, ',') + "] in the \"style\" attribute");
      }
    } catch (IllegalArgumentException ex) {
      throw new InvalidInputException("Unparseable \"style\" attribute: " + styleAttribute, ex);
    }
  }

}
