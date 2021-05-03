package org.symphonyoss.symphony.messageml.bi;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Contains all required BI data for MessageML instrumentation. It's composed by a list of {@link BiItem}, one per element
 * found inside the message along with the current messageML-utils version.
 */
public class BiContext {
  private static final Logger logger = LoggerFactory.getLogger(BiContext.class);

  private final String libraryVersion;
  private final List<BiItem> items;

  public BiContext() {
    this.libraryVersion = extractVersion();
    this.items = new ArrayList<>();
  }

  /**
   * Returns the messageML library version in use. Format is the following: "MessageML #version"
   */
  public String getLibraryVersion() {
    return libraryVersion;
  }

  public List<BiItem> getItems() {
    return items;
  }

  /**
   * Adds a specific item to the context. No check if an item with same name already exists, the item will be added in
   * any case.
   *
   * @param item to be added
   */
  public void addItem(BiItem item) {
    items.add(item);
  }

  /**
   * Add a specific item to the context. Used for simple items where we want to keep track of the size/value of the
   * entity
   *
   * @param itemName  name of the item to be added
   * @param itemValue value to be assigned
   */
  public void addItemWithValue(String itemName, Object itemValue) {
    BiItem item = new BiItem(itemName, Collections.singletonMap("count", itemValue));
    items.add(item);
  }


  /**
   * Used for simple messageML elements (like Paragraphs, Links, Headers) where we only want to keep the count of attributes found.
   * It checks if the context already has an item for the element and if that's the case it will increase the count of
   * the specific attribute found with attributeName. If context does not have the item it will create a new one with default values.
   *
   * @param itemName      name of the element to be checked
   * @param attributeName name of the attribute to be increased
   */
  public void updateItem(String itemName, String attributeName) {
    Optional<BiItem> optionalBiItem = getItemWithName(itemName);
    if (optionalBiItem.isPresent()) {
      optionalBiItem.get().increaseAttributeCount(attributeName);
    } else {
      addItem(new BiItem(itemName, attributeName));
    }
  }

  /**
   * Used for simple messageML elements (like Paragraphs, Links, Headers) where we only want to keep the count of attributes found.
   * It checks if the context already has an item for the element and if that's the case it will increase the count of
   * the specific attribute. If context does not have the item it will create a new one with default values.
   *
   * @param itemName name of the element to be checked
   */
  public void updateItem(String itemName) {
    String attributeName = "count";
    Optional<BiItem> optionalBiItem = getItemWithName(itemName);
    if (optionalBiItem.isPresent()) {
      optionalBiItem.get().increaseAttributeCount(attributeName);
    } else {
      addItem(new BiItem(itemName, attributeName));
    }
  }

  /**
   * Used for elements where we want to increase attribute's value.
   * If the element does not exist in the context yet, it is put.
   * If the element exists in the context but no value found for the attribute, it is put.
   * If the element and its attribute exist in the context, then the value is increased.
   *
   * @param itemName name of the element to be checked
   * @param attributes map of attributes for the given element
   */
  public void updateItem(String itemName, Map<String, Object> attributes) {
    Optional<BiItem> optionalBiItem = getItemWithName(itemName);
    if (optionalBiItem.isPresent()) {
      attributes.forEach((key, value) -> {
        if (!StringUtils.isEmpty(String.valueOf(value))
            && optionalBiItem.get().getAttributes().get(key) != null) {
          optionalBiItem.get().increaseAttributeCount(key);
        } else { optionalBiItem.get().getAttributes().putIfAbsent(key, value); }
      });
    } else {
      addItem(new BiItem(itemName, attributes));
    }
  }

  /**
   * Used for simple messageML elements (like Paragraphs, Links, Headers) where we only want to keep the count of attributes found.
   * It checks if the context already has an item for the element and if that's the case it will increase the count of
   * the specific attribute. If context does not have the item it will create a new one with default values.
   *
   * @param itemName name of the element to be checked
   */
  public void updateItemWithMaxValue(String itemName, Object attributeValue) {
    String attributeName = "count";
    Optional<BiItem> optionalBiItem = getItemWithName(itemName);
    if (optionalBiItem.isPresent()) {
      optionalBiItem.get().setMaxAttribute(attributeName, attributeValue);
    } else {
      Map<String, Object> attributesMap = new HashMap<>();
      attributesMap.put(attributeName, attributeValue);
      addItem(new BiItem(itemName, attributesMap));
    }
  }

  /**
   * Used to check if an attribute is present in the BI context for a given item.
   *
   * @param itemName name of the element to be checked
   * @param attributeName name of the attribute to be checked
   */
  public boolean isAttributeSet(String itemName, String attributeName) {
    Optional<BiItem> optionalBiTem = getItemWithName(itemName);
    return optionalBiTem.isPresent() && optionalBiTem.get().getAttributes() != null
        && optionalBiTem.get().getAttributes().get(attributeName) != null;
  }

  private Optional<BiItem> getItemWithName(String itemName) {
    return getItems().stream().filter(item -> item.getName().equals(itemName)).findFirst();
  }

  private static String extractVersion() {
    String version = BiContext.class.getPackage().getImplementationVersion();
    if (version == null || version.isEmpty()) {
      logger.warn("Failed to extract the messageML library version.");
      version = "Unknown";
    }
    return "MessageML " + version;
  }
}
