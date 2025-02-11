package org.finos.symphony.messageml.messagemlutils.util;

import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.exceptions.ProcessingException;
import org.finos.symphony.messageml.messagemlutils.util.instrument.resolver.InstrumentResolution;
import org.finos.symphony.messageml.messagemlutils.util.instrument.resolver.ResolutionResults;

import java.net.URI;
import java.util.List;

/**
 * A utility {@link IDataProvider} which populates {@link IUserPresentation} with the input (either user email or user ID)
 * and always validates input URLs.
 */
public class NoOpDataProvider implements IDataProvider {
  private class NoOpUserPresentation implements IUserPresentation {

    private long id;
    private final String screenName;
    private final String prettyName;
    private final String email;

    public NoOpUserPresentation(String emailAddress) {
      this.id = 0;
      this.email = emailAddress;
      this.screenName = emailAddress;
      this.prettyName = emailAddress;
    }

    public NoOpUserPresentation(Long uid) {
      this.id = uid;
      this.screenName = String.valueOf(uid);
      this.prettyName = String.valueOf(uid);
      this.email = "";
    }

    @Override
    public long getId() {
      return this.id;
    }

    @Override
    public String getScreenName() {
      return this.screenName;
    }

    @Override
    public String getPrettyName() {
      return this.prettyName;
    }

    @Override
    public String getEmail() {
      return this.email;
    }
  }

  @Override
  public IUserPresentation getUserPresentation(String emailAddress) throws InvalidInputException {
    return new NoOpUserPresentation(emailAddress);
  }

  @Override
  public IUserPresentation getUserPresentation(Long uid) throws InvalidInputException {
    return new NoOpUserPresentation(uid);
  }

  @Override
  public void validateURI(URI uri) throws InvalidInputException, ProcessingException {
    // no-op
  }

  @Override
  public ResolutionResults getFinTagPresentation(List<InstrumentResolution> uid)
      throws InvalidInputException {
    return null;
  }
}
