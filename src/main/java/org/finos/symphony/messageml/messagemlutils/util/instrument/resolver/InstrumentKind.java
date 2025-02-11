package org.finos.symphony.messageml.messagemlutils.util.instrument.resolver;

import com.fasterxml.jackson.annotation.JsonCreator;
import lombok.Getter;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum InstrumentKind {
  EQUITY("equity"),
  INDEX("index"),
  FXCROSS("fxcross");
  @Getter
  private String value;

  InstrumentKind(String value) {
    this.value = value;
  }

  @JsonCreator
  public static InstrumentKind fromValue(String text) {
    for (InstrumentKind b : InstrumentKind.values()) {
      if (String.valueOf(b.value).equals(text)) {
        return b;
      }
    }
    return null;
  }

  public static List<String> toValues() {
    return Arrays.asList(InstrumentKind.values())
        .stream()
        .map(InstrumentKind::getValue)
        .collect(Collectors.toList());
  }



}
