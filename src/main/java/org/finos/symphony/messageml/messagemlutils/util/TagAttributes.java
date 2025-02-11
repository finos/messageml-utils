package org.finos.symphony.messageml.messagemlutils.util;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TagAttributes {
  public static final String ATTR_FULLBBGCOMPTICKER = "fullbbgcompticker";
  public static final String ATTR_UNIQUEID = "unique-id";
  public static final String ATTR_FIGI = "figi";
  public static final String ATTR_BBGCOMPTICKER = "bbgcompticker";
  public static final String ATTR_FIGITICKER = "figi-ticker";
  public static final String ATTR_USCODE = "us-code";
  public static final String ATTR_ISIN = "isin";
  public static final String ATTR_LOCALCODE = "local-code";
  public static final String ATTR_INSTRUMENTCLASS = "instrument-class";
  public static final String ATTR_BBGMARKETSECTOR = "bbgmarket-sector";
  public static final String ATTR_RETURNMAINLISTING = "return-main-listing";
  public static final String ATTR_COUNTRYCODE = "country-code";
  public static final String ATTR_OPERATIONALMIC = "operational-mic";
  public static final String ATTR_FALLBACKTICKER = "fallback-ticker";


  // Identifiers
  private String fullBbgCompTicker;
  private String figi;
  private String bbgcompticker;
  private String figiTicker;
  private String uscode;
  private String isin;
  private String localcode;
  private String uniqueId;
  // Filters
  private String instrumentclass;
  private String bbgmarketsector;
  private String returnMainListing;
  private String countrycode;
  private String operationalMic;
  // Other
  private String fallbackTicker;
}
