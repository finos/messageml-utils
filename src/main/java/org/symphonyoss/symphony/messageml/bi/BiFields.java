package org.symphonyoss.symphony.messageml.bi;

/**
 * BiFields enum
 * We list all the BI elements, attributes and some values. All integers attributes have a zero default value.
 * We set a default value to be able to differentiate Integers attributes from String attributes in the agent.
 */
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
  CHIME("chimes", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  CODE("codes", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  DIV("divs", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  ENTITY("entity", BiEventType.MESSAGEML_ELEMENT_SENT),
  ENTITIES("entities", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  LINK("links", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  PARAGRAPH("paragraphs", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  PREFORMATTED("pres", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  SPAN("spans", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  TABLE("tables", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  TABLE_ROW_MAX("table_rows_max", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  TABLE_COLUMN_MAX("table_columns_max", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  TABLE_CELL_COL_SPAN("table_cells_col_span", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  TABLE_CELL_ROW_SPAN("table_cells_row_span", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  TABLE_HEADER("table_headers", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  TABLE_FOOTER("table_footers", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  EMOJIS("emojis", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  MENTIONS("mentions", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  HASHTAGS("hashtags", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  CASHTAGS("cashtags", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  BULLET_LIST("lists", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  CARD("cards", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  EXPANDABLE_CARDS_COLLAPSED("expandablecards_collapsed", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  EXPANDABLE_CARDS_CROPPED("expandablecards_cropped", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  EXPANDABLE_CARDS_EXPANDED("expandablecards_expanded", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  HEADER("headers", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  IMAGE("images", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  IMAGE_DATA("images_data", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  IMAGE_URL("images_url", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  LINE_BREAK("linebreaks", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  INPUT_STEP("input_step", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  TITLE("has_title", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  DEFAULT("default", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  PLACEHOLDER("placeholder", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  STYLE_COLOR("style_color", BiEventType.MESSAGEML_ELEMENT_SENT),
  STYLES_CUSTOM("styles_custom", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  STYLES_CLASS_TEMPO("styles_class_tempo", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  STYLES_CLASS_OTHER("styles_class_other", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  TYPE("type", BiEventType.MESSAGEML_ELEMENT_SENT),
  TYPE_MASKED_TRUE("masked", BiEventType.MESSAGEML_ELEMENT_SENT),
  TYPE_MASKED_FALSE("normal", BiEventType.MESSAGEML_ELEMENT_SENT),
  ENTITY_TYPE("entity_type", BiEventType.MESSAGEML_ELEMENT_SENT),
  MULTI_SUBMIT("is_multi_submit", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  LABEL("has_label", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  OPTIONS_COUNT("options_count", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  VALIDATION("validation", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  VALIDATION_MAX("validation_max", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  VALIDATION_MIN("validation_min", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  VALIDATION_PATTERN("validation_pattern", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  VALIDATION_OPTIONS("validation_options", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  HIGHLIGHTED_OPTIONS("highlighted_options", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  VALIDATION_STRICT("validation_strict", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  REQUIRED("required", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  MULTI_SELECT("is_multi_select", BiEventType.MESSAGEML_ELEMENT_SENT, "0"),
  MESSAGE_LENGTH("message_length", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  ENTITY_JSON_SIZE("entities_json_size", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  FREEMARKER("use_freemarker", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  POPUPS("popups", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  OPENIM("uiactions_openim", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  OPENDIALOG("uiactions_opendialog", BiEventType.MESSAGEML_MESSAGE_SENT, "0"),
  COUNT("count", BiEventType.NONE, "0");

  private final String value;
  private final BiEventType type;
  private final String defaultValue;

  BiFields(String value, BiEventType type, String defaultValue) {
    this.value = value;
    this.type = type;
    this.defaultValue = defaultValue;
  }

  BiFields(String value, BiEventType type) {
    this.value = value;
    this.type = type;
    this.defaultValue = null;
  }

  public String getValue() {
    return value;
  }

  public BiEventType getType() {
    return type;
  }

  public String getDefaultValue() {
    return defaultValue;
  }
}
