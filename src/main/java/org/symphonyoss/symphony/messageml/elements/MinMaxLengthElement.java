package org.symphonyoss.symphony.messageml.elements;

import static java.lang.String.format;

import java.util.Arrays;
import java.util.List;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

/**
 *
 * Interface to add minlength and maxlength attributes to elements
 *
 * Simply implement this interface into the element to support the two attributes
 *
 * Beware:
 * If the element implementing this interface overrides {@link Element#buildAttribute(org.symphonyoss.symphony.messageml.MessageMLParser, org.w3c.dom.Node)}
 * without calling super, it is needed to manage manually MINLENGTH_ATTR, MAXLENGTH_ATTR attributes
 *
 * @author mariacristina.dedominicis (23/07/2020)
 */

public interface MinMaxLengthElement{

    String MINLENGTH_ATTR = "minlength";
    String MAXLENGTH_ATTR = "maxlength";
    List<String> ALL_MIN_MAX_ATTRS = Arrays.asList(MINLENGTH_ATTR, MAXLENGTH_ATTR);


    /**
     * This method checks if the values assigned to minlength and maxlength attributes
     * are valid. If there is an initial value in the textarea it also checks if the latter is
     * between the range given
     *
     * @throws InvalidInputException when the attributes value are not valid or the input is not in range
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

        minLength = getDefaultValueIfCurrentIsNull(minLength, getMinValueAllowed());
        maxLength = getDefaultValueIfCurrentIsNull(maxLength, getMaxValueAllowed());

        if (isMinAndMaxLengthCombinationValid(minLength, maxLength)) {
            throw new InvalidInputException("The attribute \"minlength\" must be lower than the \"maxlength\" attribute");
        }
        validateInitialValueIfFound(minLength,maxLength);
    }

    /**
     * This method validates in case there is an initial value in the element, if that input is a
     * valid one, meaning that it respects the range of minlength and maxlength.
     *
     * @param minLength value
     * @param maxLength value
     * @throws InvalidInputException if the the initial value  is not in range
     */
    default void validateInitialValueIfFound(Integer minLength, Integer maxLength) throws InvalidInputException {
        if(hasElementInitialValue()) {
            String initialValue = getElementInitialValue();
            if (isTextSmallerThanMinLength(minLength, initialValue) || isTextBiggerThanMaxLength(maxLength, initialValue)) {
                throw new InvalidInputException(String.format(
                        "The length of this %s's initial value must be between %s and %s", getElementType(), minLength, maxLength));
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
    default boolean isMinAndMaxLengthCombinationValid(Integer minLength, Integer maxLength) {
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
        return length != null && (length < getMinValueAllowed() || length > getMaxValueAllowed());
    }

    default String getLengthErrorMessage(String attributeName) {
        return format("The attribute \"%s\" must be between %s and %s", attributeName, getMinValueAllowed(), getMaxValueAllowed());
    }

    /**
     * @return the type of the element
     */
    String getElementType();

    /**
     * @return true if the element has an initial value
     */
    boolean hasElementInitialValue();

    /**
     * @return the initial value found in the element
     */
    String getElementInitialValue();

    /**
     * @param attributeName we want to retrieve
     * @return the value corresponding to the attributeName given in input
     */
    String getAttributeValue(String attributeName);

    /**
     * @return minimum value allowed for the minLength attribute
     */
    Integer getMinValueAllowed();

    /**
     * @return maximum value allowed for the maxLength attribute
     */
    Integer getMaxValueAllowed();
}
