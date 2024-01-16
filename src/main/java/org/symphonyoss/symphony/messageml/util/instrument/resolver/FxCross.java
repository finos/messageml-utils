package org.symphonyoss.symphony.messageml.util.instrument.resolver;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;



@Data
public class FxCross extends Instrument {

  @JsonProperty("kind")
  private InstrumentKind kind;
  @JsonProperty("providerId")
  private ProviderId providerId;
  @JsonProperty("bbgMarketSector")
  private MarketSector bbgMarketSector;
  @JsonProperty("mainInstrument")
  private Boolean mainInstrument;
}
