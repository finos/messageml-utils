package org.symphonyoss.symphony.messageml.bi;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

/**
 * A BiItem can be any MessageML element such as <text-field>, <emoji>, <h1>, etc.
 */
public class BiItem {

  private static final Logger logger = LoggerFactory.getLogger(BiContext.class);

  private final String name;

  private Map<String, String> attributes;

  public BiItem(String name, Map<String, String> attributes) {
    this.name = name;
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
  public Map<String, String> getAttributes() {
    return attributes;
  }

  /**
   * If the attribute is found inside the map the corresponding value will be increased, otherwise the attribute will be
   * put in the map with value 1.
   * If the value found is not an integer, then no update will be performed.
   * @param attributeName name of the attribute to be increased in value
   */
  protected void increaseAttributeCount(String attributeName){
    int value = 1;
    if(attributes.containsKey(attributeName)){
      String valueFound = attributes.get(attributeName);
    try{
        value = Integer.parseInt(valueFound) + 1;
      } catch (NumberFormatException e){
        logger.warn("Attribute {} for element {} does not contain an integer value. The count will not be increased.",
                attributeName, getName());
        return;
      }
    }
    attributes.put(attributeName, String.valueOf(value));
  }

}
