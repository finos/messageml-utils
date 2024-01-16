package org.symphonyoss.symphony.messageml.util.instrument.resolver;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;



@Data
public class Equity extends Instrument {
  @JsonProperty("kind")
  private InstrumentKind kind;
  @JsonProperty("providerId")
  private ProviderId providerId;
  @JsonProperty("isin")
  private String isin;
  @JsonProperty("bbgCompId")
  private String bbgCompId;
  @JsonProperty("usCode")
  private String usCode;
  @JsonProperty("sedol")
  private String sedol;
  @JsonProperty("cfi")
  private String cfi;
  @JsonProperty("ric")
  private String ric;
  @JsonProperty("lei")
  private String lei;
  @JsonProperty("wkn")
  private String wkn;
  @JsonProperty("ediInstrumentId")
  private String ediInstrumentId;
  @JsonProperty("bbgMarketSector")
  private MarketSector bbgMarketSector;
  @JsonProperty("countryCode")
  private String countryCode;
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
