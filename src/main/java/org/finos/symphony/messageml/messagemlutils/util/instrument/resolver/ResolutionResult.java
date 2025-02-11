package org.finos.symphony.messageml.messagemlutils.util.instrument.resolver;



import lombok.Data;

@Data
public class ResolutionResult {
  private Instrument instrument;
  private Integer returnCode;
}
