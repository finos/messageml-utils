package org.symphonyoss.symphony.messageml.bi;

public enum BiFields {

  FORM("Form", BiEventType.MESSAGEML_ELEMENT_SENT),
  BUTTON("Button", BiEventType.MESSAGEML_ELEMENT_SENT),
  TEXT_AREA("Text_Area", BiEventType.MESSAGEML_ELEMENT_SENT),
  TEXT_FIELD("Text_Field", BiEventType.MESSAGEML_ELEMENT_SENT),
  CHECKBOX("Check_Box", BiEventType.MESSAGEML_ELEMENT_SENT),
  RADIO("Radio", BiEventType.MESSAGEML_ELEMENT_SENT),
  TIME_PICKER("Time_Picker", BiEventType.MESSAGEML_ELEMENT_SENT),
  TIMEZONE_PICKER("Time_Zone_Picker", BiEventType.MESSAGEML_ELEMENT_SENT),
  DATE_SELECTOR("Date_Selector", BiEventType.MESSAGEML_ELEMENT_SENT),
  PERSON_SELECTOR("Person_Selector", BiEventType.MESSAGEML_ELEMENT_SENT),
  SELECT("Dropdown_Menu", BiEventType.MESSAGEML_ELEMENT_SENT),
  OPTION("Option", BiEventType.MESSAGEML_ELEMENT_SENT),
  CHIME("Chimes", BiEventType.MESSAGEML_MESSAGE_SENT),
  CODE("Codes", BiEventType.MESSAGEML_MESSAGE_SENT),
  DIV("Divs", BiEventType.MESSAGEML_MESSAGE_SENT),
  ENTITY("Entity", BiEventType.MESSAGEML_ELEMENT_SENT),
  ENTITIES("Entities", BiEventType.MESSAGEML_MESSAGE_SENT),
  LINK("Links", BiEventType.MESSAGEML_MESSAGE_SENT),
  PARAGRAPH("Paragraphs", BiEventType.MESSAGEML_MESSAGE_SENT),
  PREFORMATTED("Pres", BiEventType.MESSAGEML_MESSAGE_SENT),
  SPAN("Spans", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE("Tables", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_ROW_MAX("Table_Rows_Max", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_COLUMN_MAX("Table_Columns_Max", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_CELL_COL_SPAN("Table_Cells_Col_Span", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_CELL_ROW_SPAN("Table_Cells_Row_Span", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_HEADER("Table_Headers", BiEventType.MESSAGEML_MESSAGE_SENT),
  TABLE_FOOTER("Table_Footers", BiEventType.MESSAGEML_MESSAGE_SENT),
  EMOJIS("Emojis", BiEventType.MESSAGEML_MESSAGE_SENT),
  MENTIONS("Mentions", BiEventType.MESSAGEML_MESSAGE_SENT),
  HASHTAGS("Hashtags", BiEventType.MESSAGEML_MESSAGE_SENT),
  CASHTAGS("Cashtags", BiEventType.MESSAGEML_MESSAGE_SENT),
  BULLET_LIST("Lists", BiEventType.MESSAGEML_MESSAGE_SENT),
  CARD("Cards", BiEventType.MESSAGEML_MESSAGE_SENT),
  EXPANDABLE_CARDS("Expandable_Cards", BiEventType.MESSAGEML_MESSAGE_SENT),
  EXPANDABLE_CARDS_COLLAPSED("Expandable_Cards_Collapsed", BiEventType.MESSAGEML_MESSAGE_SENT),
  EXPANDABLE_CARDS_CROPPED("Expandable_Cards_Cropped", BiEventType.MESSAGEML_MESSAGE_SENT),
  EXPANDABLE_CARDS_EXPANDED("Expandable_Cards_Expanded", BiEventType.MESSAGEML_MESSAGE_SENT),
  HEADER("Headers", BiEventType.MESSAGEML_MESSAGE_SENT),
  IMAGE("Images", BiEventType.MESSAGEML_MESSAGE_SENT),
  IMAGE_DATA("Images_Data", BiEventType.MESSAGEML_MESSAGE_SENT),
  IMAGE_URL("Images_Url", BiEventType.MESSAGEML_MESSAGE_SENT),
  LINE_BREAK("Line_Breaks", BiEventType.MESSAGEML_MESSAGE_SENT),
  INPUT_STEP("Input_Step", BiEventType.MESSAGEML_ELEMENT_SENT),
  TITLE("Title", BiEventType.MESSAGEML_ELEMENT_SENT),
  DEFAULT("Default", BiEventType.MESSAGEML_ELEMENT_SENT),
  PLACEHOLDER("Placeholder", BiEventType.MESSAGEML_ELEMENT_SENT),
  STYLE_COLOR("Style_Color", BiEventType.MESSAGEML_ELEMENT_SENT),
  STYLES_CUSTOM("Styles_Custom", BiEventType.MESSAGEML_MESSAGE_SENT),
  STYLES_CLASS_TEMPO("Styles_Class_Tempo", BiEventType.MESSAGEML_MESSAGE_SENT),
  STYLES_CLASS_OTHER("Styles_Class_Other", BiEventType.MESSAGEML_MESSAGE_SENT),
  TYPE("Type", BiEventType.MESSAGEML_ELEMENT_SENT),
  TYPE_MASKED_TRUE("Masked", BiEventType.MESSAGEML_ELEMENT_SENT),
  TYPE_MASKED_FALSE("Normal", BiEventType.MESSAGEML_ELEMENT_SENT),
  ENTITY_TYPE("Entity_Type", BiEventType.MESSAGEML_ELEMENT_SENT),
  LABEL("Label", BiEventType.MESSAGEML_ELEMENT_SENT),
  OPTIONS_COUNT("Options_Count", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION("Validation", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION_MAX("Validation_Max", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION_MIN("Validation_Min", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION_PATTERN("Validation_Pattern", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION_OPTIONS("Validation_Options", BiEventType.MESSAGEML_ELEMENT_SENT),
  HIGHLIGHTED_OPTIONS("Highlighted_Options", BiEventType.MESSAGEML_ELEMENT_SENT),
  VALIDATION_STRICT("Validation_Strict", BiEventType.MESSAGEML_ELEMENT_SENT),
  REQUIRED("Required", BiEventType.MESSAGEML_ELEMENT_SENT),
  MESSAGE_LENGTH("Message_Length", BiEventType.MESSAGEML_MESSAGE_SENT),
  ENTITY_JSON_SIZE("Entities_JSON_Size", BiEventType.MESSAGEML_MESSAGE_SENT),
  FREEMARKER("Use_FreeMarker", BiEventType.MESSAGEML_MESSAGE_SENT),
  COUNT("Count", BiEventType.NONE);

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
