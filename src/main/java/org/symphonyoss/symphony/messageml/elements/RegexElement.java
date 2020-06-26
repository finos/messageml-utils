package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.util.XmlPrintStream;
import org.w3c.dom.Node;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 *
 * Interface to add regex pattern to elements, thus the following attributes:
 *
 * pattern (the regex, it will be tested if it compiles), not mandatory
 * pattern-error-message, mandatory when pattern is added (in PresentationML it is translated to data-pattern-error-message)
 *
 * Simply implement it into the element when you want to add regex support
 *
 * Beware:
 *  If the element implementing this interface overrides {@link Element#validate()} and/or {@link Element#buildAttribute(org.symphonyoss.symphony.messageml.MessageMLParser, Node)} and/or {@link Element#asPresentationML(XmlPrintStream, org.symphonyoss.symphony.messageml.MessageMLContext)}
 *  without calling super, it is needed to manage manually PATTERN_ATTR and PATTERN_ERROR_MESSAGE_ATTR in these methods
 *
 * @author enrico.molino (12/05/2020)
 *
 */
public interface RegexElement {

  String PATTERN_ATTR = "pattern";
  String PATTERN_ERROR_MESSAGE_ATTR = "pattern-error-message";
  String PRESENTATIONML_PATTERN_ERROR_MESSAGE_ATTR = "data-pattern-error-message";
  List<String> ALL_REGEX_ATTRS = Arrays.asList(PATTERN_ATTR, PATTERN_ERROR_MESSAGE_ATTR, PRESENTATIONML_PATTERN_ERROR_MESSAGE_ATTR);

  int PATTERN_MAX_LENGTH = 256;
  int PATTERN_ERROR_MESSAGE_MAX_LENGTH = 256;

  String ATTRIBUTE_MANDATORY_WHEN_ATTRIBUTE_DEFINED_ERR = "The attribute \"%s\" is mandatory when attribute \"%s\" is defined";
  String ATTRIBUTE_TOO_LONG_ERR = "The attribute \"%s\" value is too long. Max length for this attribute is: %d";
  String REGEX_NOT_VALID_ERR = "The regex \"%s\" is not valid";

  /**
   * Validate regex (it is called automatically by {@link Element#validate() when this method is not overridden}
   *
   * @throws InvalidInputException
   */
  default void validateRegex() throws InvalidInputException {

    String regexPattern = getAttribute(PATTERN_ATTR);

    if (regexPattern != null) {
      String regexPatterErrorMessageAtt = getFormat() == FormatEnum.MESSAGEML ? PATTERN_ERROR_MESSAGE_ATTR : PRESENTATIONML_PATTERN_ERROR_MESSAGE_ATTR;
      String regexPatterErrorMessage = getAttribute(regexPatterErrorMessageAtt);
      if(regexPatterErrorMessage == null) {
        throw new InvalidInputException(String.format(ATTRIBUTE_MANDATORY_WHEN_ATTRIBUTE_DEFINED_ERR,
            regexPatterErrorMessageAtt,
            PATTERN_ATTR));
      }

      if(regexPattern.length() > PATTERN_MAX_LENGTH){
        throw new InvalidInputException(String.format(ATTRIBUTE_TOO_LONG_ERR, PATTERN_ATTR, PATTERN_MAX_LENGTH));
      }

      if(regexPatterErrorMessage.length() > PATTERN_ERROR_MESSAGE_MAX_LENGTH){
        throw new InvalidInputException(String.format(ATTRIBUTE_TOO_LONG_ERR,
            regexPatterErrorMessageAtt, PATTERN_ERROR_MESSAGE_MAX_LENGTH));
      }

      try {
        Pattern.compile(regexPattern);
      } catch (PatternSyntaxException p){
        throw new InvalidInputException(String.format(REGEX_NOT_VALID_ERR, regexPattern), p);
      }
    }
  }

  /**
   * Process MessageML attributes to build a map of regex related attributes for PresentationML
   *
   * @return a map of PresentationML regex related attributes
   */
  default Map<String, String> getRegexAttrForPresentationML(){
    Map<String, String> presentationAttrs = new LinkedHashMap<>();

    if (getAttribute(PATTERN_ATTR) != null) {
      presentationAttrs.put(PATTERN_ATTR, getAttribute(PATTERN_ATTR));
    }

    if (getAttribute(PATTERN_ERROR_MESSAGE_ATTR) != null) {
      presentationAttrs.put(PRESENTATIONML_PATTERN_ERROR_MESSAGE_ATTR, getAttribute(PATTERN_ERROR_MESSAGE_ATTR));
    }

    return presentationAttrs;
  }

  /**
   *
   * @return all element's attributes that are not regex related
   */
  default Map<String, String> getOtherAttributes(){
    Map<String, String> presentationAttrs = new LinkedHashMap<>(getAttributes());
    ALL_REGEX_ATTRS.stream().forEach(attr -> presentationAttrs.remove(attr));
    return presentationAttrs;
  }

  String getAttribute(String attr);

  Map<String, String> getAttributes();

  FormatEnum getFormat();
}
