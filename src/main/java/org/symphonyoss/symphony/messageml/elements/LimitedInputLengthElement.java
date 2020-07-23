package org.symphonyoss.symphony.messageml.elements;

import static java.lang.String.format;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

/**
 *
 * Interface to add minlength and maxlength attributes to elements
 *
 * Simply implement this interface into the element to support the two attributes
 *
 * @author mariacristina.dedominicis (23/07/2020)
 */

public interface LimitedInputLengthElement {
    String MINLENGTH_ATTR = "minlength";
    String MAXLENGTH_ATTR = "maxlength";
    Integer MIN_ALLOWED_LENGTH = 1;
    Integer MAX_ALLOWED_LENGTH = 128;

    /**
     * This method check if the input of the element respects the range given by the
     * attributes maxlength and min length
     *
     * @throws InvalidInputException when the input is not in range
     */
    default void validateMinAndMaxLengths() throws InvalidInputException {
        Integer maxLength = getAttributeAsInteger(MAXLENGTH_ATTR);
        if (isLengthOutOfRange(maxLength)) {
            throw new InvalidInputException(getLengthErrorMessage(MAXLENGTH_ATTR));
        }

        Integer minLength = getAttributeAsInteger(MINLENGTH_ATTR);
        if (isLengthOutOfRange(minLength)) {
            throw new InvalidInputException(getLengthErrorMessage(MINLENGTH_ATTR));
        }

        minLength = getDefaultValueIfCurrentIsNull(minLength, MIN_ALLOWED_LENGTH);
        maxLength = getDefaultValueIfCurrentIsNull(maxLength, MAX_ALLOWED_LENGTH);

        if (isMinAndMaxLengthCombinationValid(maxLength, minLength)) {
            throw new InvalidInputException("The attribute \"minlength\" must be lower than the \"maxlength\" attribute");
        }

        if (hasElementInitialValue()) {
            String initialValue = getElementInitialValue();
            if (isTextSmallerThanMinLength(minLength, initialValue) || isTextBiggerThanMaxLength(maxLength, initialValue)) {
                throw new InvalidInputException(String.format(
                        "The length of this " + getElementType() +"'s initial value must be between %s and %s", minLength, maxLength));
            }
        }
    }

    /**
     * In case these is no maxlenght or no minlength this method return the default value assigned
     * to each attributes
     *
     * @param currentValue is attribute as a value assigned
     * @param defaultValue default value for that attribute
     * @return
     */
    default Integer getDefaultValueIfCurrentIsNull(Integer currentValue, Integer defaultValue) {
        return currentValue == null ? defaultValue : currentValue;
    }

    /**
     * Checks if the [minlength, maxlength] range is valid
     *
     * @param maxLength value
     * @param minLength value
     * @return true is the range is valid
     */
    default boolean isMinAndMaxLengthCombinationValid(Integer maxLength, Integer minLength) {
        return minLength != null && maxLength != null && minLength > maxLength;
    }

    /**
     * @param maxLength value
     * @param text given in input
     * @return true if the input is not longer that maxlength
     */
    default boolean isTextBiggerThanMaxLength(Integer maxLength, String text) {
        return text != null && maxLength != null && text.length() > maxLength;
    }

    /**
     * @param minLength value
     * @param text given in input
     * @return true if the input is not smaller that maxlength
     */
    default boolean isTextSmallerThanMinLength(Integer minLength, String text) {
        return text != null && minLength != null && text.length() < minLength;
    }

    /**
     * This method take the value corresponding to the attribute given as input and convert
     * it to an Integer, if possible
     *
     * @param attributeName attribute to be converted
     * @return Integer version of the attribute
     * @throws InvalidInputException if the value of the attribute is not a numeric one
     */
    default Integer getAttributeAsInteger(String attributeName) throws InvalidInputException{
        Integer length = null;

        if (getAttributeValue(attributeName) != null) {
            try {
                length = Integer.parseInt(getAttributeValue(attributeName));
            } catch (NumberFormatException e) {
                throw new InvalidInputException(format("The attribute \"%s\" must be a valid number.", attributeName));
            }
        }

        return length;
    }

    /**
     * Check if the length of the input is in the default ranges of the element
     *
     * @param length of the input
     * @return
     */
    default boolean isLengthOutOfRange(Integer length) {
        return length != null && (length < MIN_ALLOWED_LENGTH || length > MAX_ALLOWED_LENGTH);
    }

    default String getLengthErrorMessage(String attributeName) {
        return format("The attribute \"%s\" must be between %s and %s", attributeName, MIN_ALLOWED_LENGTH, MAX_ALLOWED_LENGTH);
    }

    /**
     * @return the name of the element
     * Needs to be overridden to return the correct name depending of the element type
     */
    String getElementType();

    /**
     * @return true of the element has an initial value
     * Needs to be overridden
     */
    boolean hasElementInitialValue();

    /**
     * @return the initial value of the element
     * Needs to be overridden
     */
    String getElementInitialValue();

    /**
     * @param attributeName
     * @return the associated value
     * Needs to be overridden
     */
    String getAttributeValue(String attributeName);
}
