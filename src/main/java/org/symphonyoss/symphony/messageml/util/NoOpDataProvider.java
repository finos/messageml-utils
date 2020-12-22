package org.symphonyoss.symphony.messageml.util;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;
import org.symphonyoss.symphony.messageml.exceptions.ProcessingException;

import java.net.URI;

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
}
