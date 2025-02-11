package org.finos.symphony.messageml.messagemlutils.util.instrument.resolver;

import lombok.Data;

@Data
public class InstrumentResolution {
  private String resolutionId;
  private String bbgCompTicker;
  private String figi;
  private String figiTicker;
  private String uniqueId = null;
  private String isin;
  private String usCode;
  private String fullBbgCompTicker;
  private String localCode;
  private String operationalMic;
  private InstrumentKind instrumentClass;
  private String countryCode;
  private String returnMainListing;
  private MarketSector bbgMarketSector;

}

