package org.symphonyoss.symphony.messageml.util.instrument.resolver;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class Index extends Instrument {
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
}
