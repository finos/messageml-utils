package org.symphonyoss.symphony.messageml.bi;

public enum BiFields {

  FORM("Form"),
  BUTTON("Button"),
  TEXT_AREA("TextArea"),
  TEXT_FIELD("TextField"),
  CHECKBOX("CheckBox"),
  RADIO("Radio"),
  TIME_PICKER("TimePicker"),
  TIMEZONE_PICKER("TimeZonePicker"),
  DATE_SELECTOR("DateSelector"),
  PERSON_SELECTOR("PersonSelector"),
  SELECT("Dropdown Menu"),
  OPTION("Option"),
  CHIME("Chimes"),
  CODE("Codes"),
  DIV("Divs"),
  ENTITY("Entity"),
  ENTITIES("Entities"),
  LINK("Links"),
  PARAGRAPH("Paragraphs"),
  PREFORMATTED("Pres"),
  SPAN("Spans"),
  TABLE("Tables"),
  TABLE_ROW_MAX("TableRowsMax"),
  TABLE_COLUMN_MAX("TableColumnsMax"),
  TABLE_CELL_COL_SPAN("TableCellsColSpan"),
  TABLE_CELL_ROW_SPAN("TableCellsRowSpan"),
  TABLE_HEADER("TableHeaders"),
  TABLE_FOOTER("TableFooters"),
  EMOJIS("Emojis"),
  MENTIONS("Mentions"),
  HASHTAGS("Hashtags"),
  CASHTAGS("Cashtags"),
  BULLET_LIST("Lists"),
  CARD("Cards"),
  EXPANDABLE_CARDS("ExpandableCards"),
  EXPANDABLE_CARDS_COLLAPSED("ExpandableCardsCollapsed"),
  EXPANDABLE_CARDS_CROPPED("ExpandableCardsCropped"),
  EXPANDABLE_CARDS_EXPANDED("ExpandableCardsExpanded"),
  HEADER("Headers"),
  IMAGE("Images"),
  IMAGE_DATA("ImagesData"),
  IMAGE_URL("ImagesUrl"),
  LINE_BREAK("LineBreaks"),
  INPUT_STEP("Input_Step"),
  TITLE("Title"),
  DEFAULT("Default"),
  PLACEHOLDER("Placeholder"),
  STYLE_COLOR("StyleColor"),
  STYLES_CUSTOM("StylesCustom"),
  STYLES_CLASS_TEMPO("StylesClassTempo"),
  STYLES_CLASS_OTHER("StylesClassOther"),
  TYPE("Type"),
  TYPE_MASKED_TRUE("Masked"),
  TYPE_MASKED_FALSE("Normal"),
  ENTITY_TYPE("EntityType"),
  LABEL("Label"),
  OPTIONS_COUNT("OptionsCount"),
  VALIDATION("Validation"),
  VALIDATION_MAX("Validation_Max"),
  VALIDATION_MIN("Validation_Min"),
  VALIDATION_PATTERN("Validation_Pattern"),
  VALIDATION_OPTIONS("Validation_Options"),
  HIGHLIGHTED_OPTIONS("HighlightedOptions"),
  VALIDATION_STRICT("Validation_Strict"),
  REQUIRED("Required"),
  MESSAGE_LENGTH("MessageLength"),
  ENTITY_JSON_SIZE("EntitiesJSONSize"),
  FREEMARKER("UseFreeMarker"),
  COUNT("count");

  private final String value;

  BiFields(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

}
