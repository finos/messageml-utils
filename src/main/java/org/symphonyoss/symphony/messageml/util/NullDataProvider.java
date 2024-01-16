package org.symphonyoss.symphony.messageml.util;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;
import org.symphonyoss.symphony.messageml.util.instrument.resolver.InstrumentResolution;
import org.symphonyoss.symphony.messageml.util.instrument.resolver.ResolutionResults;

import java.net.URI;
import java.util.List;

/**
 * A utility {@link IDataProvider} which populates {@link IUserPresentation} with empty strings and a user ID of 0 for every input
 * and always validates input URLs.
 */
public class NullDataProvider implements IDataProvider {
  private class NullUserPresentation implements IUserPresentation {

    @Override
    public long getId() {
      return 0;
    }

    @Override
    public String getScreenName() {
      return "";
    }

    @Override
    public String getPrettyName() {
      return "";
    }

    @Override
    public String getEmail() {
      return "";
    }
  }

  @Override
  public IUserPresentation getUserPresentation(String emailAddress) throws InvalidInputException {
    return new NullUserPresentation();
  }

  @Override
  public IUserPresentation getUserPresentation(Long uid) throws InvalidInputException {
    return new NullUserPresentation();
  }

  @Override
  public void validateURI(URI uri) throws InvalidInputException, ProcessingException {
    // no-op
  }

  @Override
  public ResolutionResults getFinTagPresentation(List<InstrumentResolution> criteria)
      throws InvalidInputException {
    return null;
  }
}
