package org.finos.symphony.messageml.messagemlutils.util.instrument.resolver;

import lombok.Data;

import java.util.Map;



@Data
public class ResolutionResults {
  private Map<String, ResolutionResult> instruments;

}

