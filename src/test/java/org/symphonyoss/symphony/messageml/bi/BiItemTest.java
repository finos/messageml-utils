package org.symphonyoss.symphony.messageml.bi;

import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class BiItemTest {

  @Test
  public void testIncreaseAttributeCountWhenAttributeNotFound() {
    String attributeName = "attribute1";
    BiItem biItem = new BiItem("element1", new HashMap<>());
    biItem.increaseAttributeCount(attributeName);

    assertEquals(1, biItem.getAttributes().size());
    assertTrue(biItem.getAttributes().containsKey(attributeName));
    assertEquals(1, biItem.getAttributes().get(attributeName));
  }

  @Test
  public void testIncreaseAttributeCountWhenAttributeFound() {
    String attributeName = "attribute1";
    Map<String, Object> attributes = new HashMap<>();
    attributes.put(attributeName, 3);
    BiItem biItem = new BiItem("element1", attributes);
    biItem.increaseAttributeCount(attributeName);

    assertEquals(1, biItem.getAttributes().size());
    assertTrue(biItem.getAttributes().containsKey(attributeName));
    assertEquals(4, biItem.getAttributes().get(attributeName));
  }

  @Test
  public void testIncreaseAttributeCountWhenInvalidFormat() {
    String attributeName = "attribute1";
    Map<String, Object> attributes = new HashMap<>();
    attributes.put(attributeName, "notInteger");
    BiItem biItem = new BiItem("element1", attributes);
    biItem.increaseAttributeCount(attributeName);

    //nothing should change
    assertEquals(1, biItem.getAttributes().size());
    assertTrue(biItem.getAttributes().containsKey(attributeName));
    assertEquals("notInteger", biItem.getAttributes().get(attributeName));
  }

  @Test
  public void testSetMaxAttributeWhenAttributeNotFound() {
    String attributeName = "attribute1";
    BiItem biItem = new BiItem("element1", new HashMap<>());
    biItem.setMaxAttribute(attributeName, 4);

    assertEquals(1, biItem.getAttributes().size());
    assertTrue(biItem.getAttributes().containsKey(attributeName));
    assertEquals(4, biItem.getAttributes().get(attributeName));
  }

  @Test
  public void testSetMaxAttributeWhenAttributeFound() {
    String attributeName = "attribute1";
    Map<String, Object> attributes = new HashMap<>();
    attributes.put(attributeName, 3);

    BiItem biItem = new BiItem("element1", attributes);
    assertEquals(1, biItem.getAttributes().size());
    assertTrue(biItem.getAttributes().containsKey(attributeName));
    assertEquals(3, biItem.getAttributes().get(attributeName));

    biItem.setMaxAttribute(attributeName, 8);

    assertEquals(8, biItem.getAttributes().get(attributeName));
  }

  @Test
  public void testSetMaxAttributetWhenInvalidFormat() {
    String attributeName = "attribute1";
    Map<String, Object> attributes = new HashMap<>();
    attributes.put(attributeName, "notInteger");
    BiItem biItem = new BiItem("element1", attributes);
    biItem.setMaxAttribute(attributeName, 4);

    //nothing should change
    assertEquals(1, biItem.getAttributes().size());
    assertTrue(biItem.getAttributes().containsKey(attributeName));
    assertEquals("notInteger", biItem.getAttributes().get(attributeName));
  }

}
