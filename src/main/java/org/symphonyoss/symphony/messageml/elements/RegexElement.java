package org.symphonyoss.symphony.messageml.elements;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * @author enrico.molino (12/05/2020)
 *
 * Interface to add regex pattern to elements
 * Simply implement it into the element you want to add regex support
 *
 * Beware: It the element implementing this interfaces overrides {@link Element#buildAttribute} without calling super, it is needed to manage manually PATTERN_ATTR and PATTERN_ERROR_MESSAGE_ATTR
 *
 */
public interface RegexElement {

  static final String PATTERN_ATTR = "pattern";
  static final String PATTERN_ERROR_MESSAGE_ATTR = "pattern-error-message";
  static final List<String> PATTERN_ATTRS = Arrays.asList(PATTERN_ATTR, PATTERN_ERROR_MESSAGE_ATTR);

  static final int PATTERN_MAX_LENGTH = 256;
  static final int PATTERN_ERROR_MESSAGE_MAX_LENGTH = 256;

  static final String ATTRIBUTE_MANDATORY_WHEN_ATTRIBUTE_DEFINED_ERR = "The attribute \"%s\" is mandatory when attribute \"%s\" is defined";
  static final String ATTRIBUTE_TOO_LONG_ERR = "The attribute \"%s\" value is too long. Max length for this attribute is: %d";
  static final String REGEX_NOT_VALID_ERR = "The regex \"%s\" is not valid";

  /**
   * Validate regex (it is called automatically {@link Element#validate()}
   *
   * @throws InvalidInputException
   */
  default void validateRegex() throws InvalidInputException {

    String regexPattern = getAttribute(PATTERN_ATTR);

    if (regexPattern != null) {
      String regexPatterErrorMessage = getAttribute(PATTERN_ERROR_MESSAGE_ATTR);
      if(regexPatterErrorMessage == null) {
        throw new InvalidInputException(String.format(ATTRIBUTE_MANDATORY_WHEN_ATTRIBUTE_DEFINED_ERR,
            PATTERN_ERROR_MESSAGE_ATTR,
            PATTERN_ATTR));
      }

      if(regexPattern.length() > PATTERN_MAX_LENGTH){
        throw new InvalidInputException(String.format(ATTRIBUTE_TOO_LONG_ERR, PATTERN_ATTR, PATTERN_MAX_LENGTH));
      }

      if(regexPatterErrorMessage.length() > PATTERN_ERROR_MESSAGE_MAX_LENGTH){
        throw new InvalidInputException(String.format(ATTRIBUTE_TOO_LONG_ERR,
            PATTERN_ERROR_MESSAGE_ATTR, PATTERN_ERROR_MESSAGE_MAX_LENGTH));
      }

      try {
        Pattern.compile(regexPattern);
      } catch (PatternSyntaxException p){
        throw new InvalidInputException(String.format(REGEX_NOT_VALID_ERR, regexPattern), p);
      }
    }
  }

  String getAttribute(String attr);

}
