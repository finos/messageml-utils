package org.symphonyoss.symphony.messageml.util.instrument.resolver;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import lombok.Getter;
import lombok.Setter;


@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.EXISTING_PROPERTY,
    property = "kind", visible = true)
@JsonSubTypes({
    @JsonSubTypes.Type(value = Equity.class, name = "equity"),
    @JsonSubTypes.Type(value = Index.class, name = "index"),
    @JsonSubTypes.Type(value = FxCross.class, name = "fxcross"),
})
@Getter
@Setter
public class Instrument {
  @JsonProperty("uniqueId")
  private String uniqueId;
  @JsonProperty("rootBbgCompTicker")
  private String rootBbgCompTicker;
  @JsonProperty("fullBbgCompTicker")
  private String fullBbgCompTicker;
  @JsonProperty("bbgCompTicker")
  private String bbgCompTicker;
  @JsonProperty("figi")
  private String figi;
  @JsonProperty("figiTicker")
  private String figiTicker;
  @JsonProperty("localCode")
  private String localCode;
  @JsonProperty("instrumentTypeCode")
  private String instrumentTypeCode;
  @JsonProperty("instrumentTypeName")
  private String instrumentTypeName;
  @JsonProperty("displayName")
  private String displayName;
  @JsonProperty("currency")
  private String currency;
}
