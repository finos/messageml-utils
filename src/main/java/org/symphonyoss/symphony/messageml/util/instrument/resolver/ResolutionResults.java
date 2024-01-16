package org.symphonyoss.symphony.messageml.util.instrument.resolver;

import lombok.Data;

import java.util.Map;



@Data
public class ResolutionResults {
  private Map<String, ResolutionResult> instruments;

}

