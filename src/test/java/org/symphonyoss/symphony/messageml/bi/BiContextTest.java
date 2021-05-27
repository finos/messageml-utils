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
    biContext.updateItemCount(BiFields.RADIO.getValue(), BiFields.BUTTON.getValue());

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals(BiFields.RADIO.getValue(), biItem.getName());
    assertEquals("{button=1}", biItem.getAttributes().toString());
  }

  @Test
  public void testUpdateItemWithAttrNameWhenItemAndAttributeFound() {
    Map<String, Object> linkAttrs = new HashMap<>();
    linkAttrs.put(BiFields.BUTTON.getValue(), 1);
    BiItem headerItem = new BiItem(BiFields.RADIO.getValue(), linkAttrs);
    biContext.addItem(headerItem);

    biContext.updateItemCount(BiFields.RADIO.getValue(), BiFields.BUTTON.getValue());

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals(BiFields.RADIO.getValue(), biItem.getName());
    assertEquals("{button=2}", biItem.getAttributes().toString());
  }

  @Test
  public void testUpdateItemInContextWhenItemNotFound() {
    biContext.updateItemCount(BiFields.LINK.getValue());

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals(BiFields.LINK.getValue(), biItem.getName());
    assertEquals("{count=1}", biItem.getAttributes().toString());
  }

  @Test
  public void testUpdateItemInContextWhenItemAndAttributeFound() {
    Map<String, Object> linkAttrs = new HashMap<>();
    linkAttrs.put(BiFields.COUNT.getValue(), 1);
    BiItem headerItem = new BiItem(BiFields.LINK.getValue(), linkAttrs);
    biContext.addItem(headerItem);

    biContext.updateItemCount(BiFields.LINK.getValue());

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals(BiFields.LINK.getValue(), biItem.getName());
    assertEquals("{count=2}", biItem.getAttributes().toString());
  }

  @Test
  public void testAddItemWithValue() {
    biContext.addItemWithValue(BiFields.ENTITY_JSON_SIZE.getValue(), 1000);

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals(BiFields.ENTITY_JSON_SIZE.getValue(), biItem.getName());
    assertEquals("{count=1000}", biItem.getAttributes().toString());
  }

  @Test
  public void testUpdateItemWithMaxValue() {
    biContext.updateItemWithMaxValue(BiFields.TABLE_COLUMN_MAX.getValue(), 4);
    biContext.updateItemWithMaxValue(BiFields.TABLE_COLUMN_MAX.getValue(), 6);
    biContext.updateItemWithMaxValue(BiFields.TABLE_COLUMN_MAX.getValue(), 1);

    assertEquals(1, biContext.getItems().size());

    BiItem biItem = biContext.getItems().get(0);
    assertEquals(BiFields.TABLE_COLUMN_MAX.getValue(), biItem.getName());
    assertEquals("{count=6}", biItem.getAttributes().toString());
  }

}
