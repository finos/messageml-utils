package org.symphonyoss.symphony.messageml.bi;

import org.junit.Before;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;

public class BiContextTest {

  public BiContext biContext;

  @Before
  public void setUp() {
    biContext = new BiContext();
  }

  @Test
  public void testUpdateItemWithAttrNameInContextWhenItemNotFound() {
    biContext.updateItem("RadioButton", "button");

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals("RadioButton", biItem.getName());
    assertEquals("{button=1}", biItem.getAttributes().toString());
  }

  @Test
  public void testUpdateItemWithAttrNameWhenItemAndAttributeFound() {
    Map<String, Object> linkAttrs = new HashMap<>();
    linkAttrs.put("button", 1);
    BiItem headerItem = new BiItem("RadioButton", linkAttrs);
    biContext.addItem(headerItem);

    biContext.updateItem("RadioButton", "button");

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals("RadioButton", biItem.getName());
    assertEquals("{button=2}", biItem.getAttributes().toString());
  }

  @Test
  public void testUpdateItemInContextWhenItemNotFound() {
    biContext.updateItem("Link");

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals("Link", biItem.getName());
    assertEquals("{count=1}", biItem.getAttributes().toString());
  }

  @Test
  public void testUpdateItemInContextWhenItemAndAttributeFound() {
    Map<String, Object> linkAttrs = new HashMap<>();
    linkAttrs.put("count", 1);
    BiItem headerItem = new BiItem("Link", linkAttrs);
    biContext.addItem(headerItem);

    biContext.updateItem("Link");

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals("Link", biItem.getName());
    assertEquals("{count=2}", biItem.getAttributes().toString());
  }

  @Test
  public void testAddItemWithValue() {
    biContext.addItemWithValue("EntityJSONSize", 1000);

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals("EntityJSONSize", biItem.getName());
    assertEquals("{count=1000}", biItem.getAttributes().toString());
  }

  @Test
  public void testUpdateItemWithMaxValue() {
    biContext.updateItemWithMaxValue("TableMaxColumns", 4);
    biContext.updateItemWithMaxValue("TableMaxColumns", 6);
    biContext.updateItemWithMaxValue("TableMaxColumns", 1);

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals("TableMaxColumns", biItem.getName());
    assertEquals("{count=6}", biItem.getAttributes().toString());
  }

}
