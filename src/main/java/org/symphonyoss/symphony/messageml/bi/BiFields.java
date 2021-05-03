package org.symphonyoss.symphony.messageml.bi;

public enum BiFields {

  FORM("Form", "form"),
  BUTTON("Button", "button"),
  TEXT_AREA("TextArea", "textarea"),
  TEXT_FIELD("TextField", "text-field"),
  CHECKBOX("CheckBox", "checkbox"),
  RADIO("Radio", "radio"),
  TIME_PICKER("TimePicker", "time-picker"),
  TIMEZONE_PICKER("TimeZonePicker", "timezone-picker"),
  DATE_SELECTOR("DateSelector", "date-picker"),
  PERSON_SELECTOR("PersonSelector", "person-selector"),
  SELECT("Dropdown Menu", "select"),
  OPTION("Option", "option"),
  CHIME("Chimes", "chime"),
  CODE("Codes", "code"),
  DIV("Divs", "div"),
  ENTITY("Entity", "entity"),
  LINK("Links", null),
  PARAGRAPH("Paragraphs", "p"),
  PREFORMATTED("Pres", "pre"),
  SPAN("Spans", "span"),
  TABLE("Tables", "table"),
  TABLE_ROW_MAX("TableRowsMax", null),
  TABLE_COLUMN_MAX("TableColumnsMax", null),
  TABLE_CELL_COL_SPAN("TableCellsColSpan", null),
  TABLE_CELL_ROW_SPAN("TableCellsRowSpan", null),
  TABLE_HEADER("TableHeaders", null),
  TABLE_FOOTER("TableFooters", null),
  EMOJIS("Emojis", "emoji"),
  MENTIONS("Mentions", null),
  HASHTAGS("Hashtags", null),
  CASHTAGS("Cashtags", null),
  BULLET_LIST("Lists", "ul"),
  CARD("Cards", "card"),
  EXPANDABLE_CARDS("ExpandableCards", "expandable-card"),
  EXPANDABLE_CARDS_COLLAPSED("ExpandableCardsCollapsed", null),
  EXPANDABLE_CARDS_CROPPED("ExpandableCardsCropped", null),
  EXPANDABLE_CARDS_EXPANDED("ExpandableCardsExpanded", null),
  HEADER("Headers", null),
  IMAGE("Images", "img"),
  IMAGE_DATA("ImagesData", null),
  IMAGE_URL("ImagesUrl", null),
  LINE_BREAK("LineBreaks", "br"),
  INPUT_STEP("Input_Step", "step"),
  TITLE("Title", "title"),
  DEFAULT("Default", "default"),
  PLACEHOLDER("Placeholder", "placeholder"),
  STYLE_COLOR("StyleColor", "class"),
  STYLES_CUSTOM("StylesCustom", "style"),
  STYLES_CLASS_TEMPO("StylesClassTempo", null),
  STYLES_CLASS_OTHER("StylesClassOther", null),
  TYPE("Type", "type"),
  TYPE_MASKED_TRUE("Masked", null),
  TYPE_MASKED_FALSE("Normal", null),
  ENTITY_TYPE("EntityType", "type"),
  LABEL("Label", "label"),
  OPTIONS_COUNT("OptionsCount", null),
  VALIDATION("Validation", null),
  VALIDATION_MAX("Validation_Max", null),
  VALIDATION_MIN("Validation_Min", null),
  VALIDATION_PATTERN("Validation_Pattern", null),
  VALIDATION_OPTIONS("Validation_Options", null),
  HIGHLIGHTED_OPTIONS("HighlightedOptions", null),
  VALIDATION_STRICT("Validation_Strict", null),
  REQUIRED("Required", "required"),
  MESSAGE_LENGTH("MessageLength", null),
  ENTITY_JSON_SIZE("EntitiesJSONSize", null),
  COUNT("count", null);

  private String fieldName;
  private String messageMlAttribute;

  BiFields(String fieldName, String messageMlAttribute) {
    this.fieldName = fieldName;
    this.messageMlAttribute = messageMlAttribute;
  }

  public String getFieldName() {
    return fieldName;
  }

  public String getMessageMlAttribute() {
    return messageMlAttribute;
  }

}
