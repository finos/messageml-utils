package org.symphonyoss.symphony.messageml.bi;

import joptsimple.internal.Strings;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

/**
 * A BiItem can be any MessageML element such as <text-field>, <emoji>, <h1>, etc.
 */
public class BiItem {

  private static final Logger logger = LoggerFactory.getLogger(BiContext.class);

  private final String name;
  private final Map<String, Object> attributes;

  public BiItem(String name, Map<String, Object> attributes) {
    this.name = name;
    this.attributes = attributes;
  }

  /**
   * Constructor which takes an item name and an attribute name and initialize a map of attribute
   * setting the attribute value by default to 1.
   * @param name ot the item to be created
   * @param attribute name of the attribute to be included in the attributes map
   */
  public BiItem(String name, String attribute) {
    this.name = name;
    Map<String, Object> attributes =  new HashMap<>();
    attributes.put(attribute, 1);
    this.attributes = attributes;

  }

  /**
   * Returns the name of the MessageML element
   */
  public String getName() {
    return name;
  }

  /**
   * Returns a map containing all the attributes for a specific MessageML element.
   * The map is composed by a key/value pair where the key is the name of the attribute and the value is either the number
   * of occurrences for generic attributes of the value or the attribute it self (e.g Button type can be either "action" or "reset").
   * If a specific attribute is not present the respective value will be set to an empty string.
   */
  public Map<String, Object> getAttributes() {
    return attributes;
  }

  /**
   * If the attribute is found inside the map the corresponding value will be increased, otherwise the attribute will be
   * put in the map with value 1.
   * If the value found is not an integer, then no update will be performed.
   *
   * @param attributeName name of the attribute to be increased in value
   */
  protected void increaseAttributeCount(String attributeName) {
    try {
      Integer value = getZeroIfEmptyString(attributeName);
      value ++;
      attributes.put(attributeName, value);
    } catch (ClassCastException e) {
      logger.warn("Attribute {} for element {} does not contain an integer value. The count will not be increased.",
              attributeName, getName());
    }
  }

  private Integer getZeroIfEmptyString(String attributeName) {
    Object value = attributes.getOrDefault(attributeName, 0);
    if (value.equals(StringUtils.EMPTY)) {
      return 0;
    }
    return (Integer) value;
  }
}
