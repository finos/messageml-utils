package org.symphonyoss.symphony.messageml.bi;

public enum BiFields {

  FORM("form", BiEventType.MESSAGEML_ELEMENT_SENT),
  BUTTON("button", BiEventType.MESSAGEML_ELEMENT_SENT),
  TEXT_AREA("textarea", BiEventType.MESSAGEML_ELEMENT_SENT),
  TEXT_FIELD("textfield", BiEventType.MESSAGEML_ELEMENT_SENT),
  CHECKBOX("checkbox", BiEventType.MESSAGEML_ELEMENT_SENT),
  RADIO("radio", BiEventType.MESSAGEML_ELEMENT_SENT),
  TIME_PICKER("timepicker", BiEventType.MESSAGEML_ELEMENT_SENT),
  TIMEZONE_PICKER("timezonepicker", BiEventType.MESSAGEML_ELEMENT_SENT),
  DATE_SELECTOR("dateselector", BiEventType.MESSAGEML_ELEMENT_SENT),
  PERSON_SELECTOR("personselector", BiEventType.MESSAGEML_ELEMENT_SENT),
  SELECT("dropdownmenu", BiEventType.MESSAGEML_ELEMENT_SENT),
  OPTION("option", BiEventType.MESSAGEML_ELEMENT_SENT),
  CHIME("chimes", BiEventType.MESSAGEML_MESSAGE_SENT),
  CODE("codes", BiEventType.MESSAGEML_MESSAGE_SENT),
  DIV("divs", BiEventType.MESSAGEML_MESSAGE_SENT),
  ENTITY("entity", BiEventType.MESSAGEML_ELEMENT_SENT),
  ENTITIES("entities", BiEventType.MESSAGEML_MESSAGE_SENT),
  LINK("links", BiEventType.MESSAGEML_MESSAGE_SENT),
  PARAGRAPH("paragraphs", BiEventType.MESSAGEML_MESSAGE_SENT),
  PREFORMATTED("pres", BiEventType.MESSAGEML_MESSAGE_SENT),
  SPAN("spans", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE("tables", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_ROW_MAX("table_rows_max", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_COLUMN_MAX("table_columns_max", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_CELL_COL_SPAN("table_cells_col_span", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_CELL_ROW_SPAN("table_cells_row_span", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_HEADER("table_headers", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_FOOTER("table_footers", BiEventType.MESSAGEML_MESSAGE_SENT),
  EMOJIS("emojis", BiEventType.MESSAGEML_MESSAGE_SENT),
  MENTIONS("mentions", BiEventType.MESSAGEML_MESSAGE_SENT),
  HASHTAGS("hashtags", BiEventType.MESSAGEML_MESSAGE_SENT),
  CASHTAGS("cashtags", BiEventType.MESSAGEML_MESSAGE_SENT),
  BULLET_LIST("lists", BiEventType.MESSAGEML_MESSAGE_SENT),
  CARD("cards", BiEventType.MESSAGEML_MESSAGE_SENT),
  EXPANDABLE_CARDS("expandablecards", BiEventType.MESSAGEML_MESSAGE_SENT),
  EXPANDABLE_CARDS_COLLAPSED("expandablecards_collapsed", BiEventType.MESSAGEML_MESSAGE_SENT),
  EXPANDABLE_CARDS_CROPPED("expandablecards_cropped", BiEventType.MESSAGEML_MESSAGE_SENT),
  EXPANDABLE_CARDS_EXPANDED("expandablecards_expanded", BiEventType.MESSAGEML_MESSAGE_SENT),
  HEADER("headers", BiEventType.MESSAGEML_MESSAGE_SENT),
  IMAGE("images", BiEventType.MESSAGEML_MESSAGE_SENT),
  IMAGE_DATA("images_data", BiEventType.MESSAGEML_MESSAGE_SENT),
  IMAGE_URL("images_url", BiEventType.MESSAGEML_MESSAGE_SENT),
  LINE_BREAK("linebreaks", BiEventType.MESSAGEML_MESSAGE_SENT),
  INPUT_STEP("input_step", BiEventType.MESSAGEML_ELEMENT_SENT),
  TITLE("has_title", BiEventType.MESSAGEML_ELEMENT_SENT),
  DEFAULT("default", BiEventType.MESSAGEML_ELEMENT_SENT),
  PLACEHOLDER("placeholder", BiEventType.MESSAGEML_ELEMENT_SENT),
  STYLE_COLOR("style_color", BiEventType.MESSAGEML_ELEMENT_SENT),
  STYLES_CUSTOM("styles_custom", BiEventType.MESSAGEML_MESSAGE_SENT),
  STYLES_CLASS_TEMPO("styles_class_tempo", BiEventType.MESSAGEML_MESSAGE_SENT),
  STYLES_CLASS_OTHER("styles_class_other", BiEventType.MESSAGEML_MESSAGE_SENT),
  TYPE("type", BiEventType.MESSAGEML_ELEMENT_SENT),
  TYPE_MASKED_TRUE("masked", BiEventType.MESSAGEML_ELEMENT_SENT),
  TYPE_MASKED_FALSE("normal", BiEventType.MESSAGEML_ELEMENT_SENT),
  ENTITY_TYPE("entity_type", BiEventType.MESSAGEML_ELEMENT_SENT),
  LABEL("has_label", BiEventType.MESSAGEML_ELEMENT_SENT),
  OPTIONS_COUNT("options_count", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION("validation", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION_MAX("validation_max", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION_MIN("validation_min", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION_PATTERN("validation_pattern", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION_OPTIONS("validation_options", BiEventType.MESSAGEML_ELEMENT_SENT),
  HIGHLIGHTED_OPTIONS("highlighted_options", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION_STRICT("validation_strict", BiEventType.MESSAGEML_ELEMENT_SENT),
  REQUIRED("required", BiEventType.MESSAGEML_ELEMENT_SENT),
  MESSAGE_LENGTH("message_length", BiEventType.MESSAGEML_MESSAGE_SENT),
  ENTITY_JSON_SIZE("entities_json_size", BiEventType.MESSAGEML_MESSAGE_SENT),
  FREEMARKER("use_freemarker", BiEventType.MESSAGEML_MESSAGE_SENT),
  COUNT("count", BiEventType.NONE);

  private final String value;
  private final BiEventType type;

  BiFields(String value, BiEventType type) {
    this.value = value;
    this.type = type;
  }

  public String getValue() {
    return value;
  }

  public BiEventType getType() {
    return type;
  }
}
