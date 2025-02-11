package org.finos.symphony.messageml.messagemlutils.util.instrument.resolver;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

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
  @JsonProperty("kind")
  private InstrumentKind kind;
  @JsonProperty("providerId")
  private ProviderId providerId;
  @JsonProperty("isin")
  private String isin;
  @JsonProperty("ric")
  private String ric;
  @JsonProperty("wkn")
  private String wkn;
  @JsonProperty("ediInstrumentId")
  private String ediInstrumentId;
  @JsonProperty("bbgMarketSector")
  private MarketSector bbgMarketSector;
  @JsonProperty("countryCode")
  private String countryCode;
  @JsonProperty("mainInstrument")
  private Boolean mainInstrument;
  @JsonProperty("bbgCompId")
  private String bbgCompId;
  @JsonProperty("usCode")
  private String usCode;
  @JsonProperty("sedol")
  private String sedol;
  @JsonProperty("cfi")
  private String cfi;
  @JsonProperty("lei")
  private String lei;
  @JsonProperty("countryName")
  private String countryName;
  @JsonProperty("exchangeName")
  private String exchangeName;
  @JsonProperty("ediExchangeCode")
  private String ediExchangeCode;
  @JsonProperty("primaryExchange")
  private Boolean primaryExchange;
  @JsonProperty("operationalMic")
  private String operationalMic;
}
