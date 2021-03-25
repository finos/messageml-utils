package org.symphonyoss.symphony.messageml.bi;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.model.Model;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileReader;
import java.io.IOException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Contains all required BI data for MessageML instrumentation. It's composed by a list of {@link BiItem}, one per element
 * found inside the message along with the current messageML-utils version.
 */
public class BiContext {

  private final String libraryVersion;

  private final List<BiItem> items;

  private static final Logger logger = LoggerFactory.getLogger(BiContext.class);

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
   * @param item to be added
   */
  public void addItem(BiItem item) {
    items.add(item);
  }

  /**
   * Used for simple messageML elements (like Paragraphs, Links, Headers) where we only want to keep the count of attributes found.
   * It checks if the context already has an item for the element and if that's the case it will increase the count of
   * the specific attribute. If context does not have the item it will create a new one with default values.
   * @param itemName name of the element to be checked
   * @param attributeName name of the attribute to be increased
   */
  public void updateItemInContext(String itemName, String attributeName) {
    Map<String, String> attributes =  new HashMap<>();
    Optional<BiItem> optionalBiItem = getItemWithName(itemName);
    if(optionalBiItem.isPresent()){
      BiItem item = optionalBiItem.get();
      items.remove(item);
      item.increaseAttributeCount(attributeName);
      addItem(item);
    } else {
      attributes.put(attributeName, String.valueOf(1));
      addItem(new BiItem(itemName, attributes));
    }
  }

  private Optional<BiItem> getItemWithName(String itemName){
    for(BiItem item: getItems()){
      if(item.getName().equals(itemName)){
        return Optional.of(item);
      }
    }
    return Optional.empty();
  }

  private String extractVersion(){
    StringBuilder version = new StringBuilder("MessageML ");
    MavenXpp3Reader reader = new MavenXpp3Reader();
    Model model;
    try {
      model = reader.read(new FileReader("pom.xml"));
    } catch (IOException | XmlPullParserException e) {
      logger.warn("Failed to extract the messageML library version.");
      return StringUtils.EMPTY;
    }
    if(model != null){
      version.append(model.getVersion());
    }
    return version.toString();
  }
}