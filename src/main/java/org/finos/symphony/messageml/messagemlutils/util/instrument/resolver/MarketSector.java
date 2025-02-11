package org.finos.symphony.messageml.messagemlutils.util.instrument.resolver;


import com.fasterxml.jackson.annotation.JsonCreator;
import lombok.Getter;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum MarketSector {
  EQUITY("Equity"),
  COMDTY("Comdty"),
  CORP("Corp"),
  CURNCY("Curncy"),
  GOVT("Govt"),
  INDEX("Index"),
  MMKT("Mmkt"),
  MTGE("Mtge"),
  MUNI("Muni"),
  PFD("Pfd");
  @Getter
  private String value;

  MarketSector(String value) {
    this.value = value;
  }

  @JsonCreator
  public static MarketSector fromValue(String text) {
    for (MarketSector b : MarketSector.values()) {
      if (String.valueOf(b.value).equals(text)) {
        return b;
      }
    }
    return null;
  }

  public static List<String> toValues() {
    return Arrays.asList(MarketSector.values())
        .stream()
        .map(MarketSector::getValue)
        .collect(Collectors.toList());
  }

}
