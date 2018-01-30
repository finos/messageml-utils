/*
 * Copyright 2016-2017 MessageML - Symphony LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.symphonyoss.symphony.messageml.util;

import org.symphonyoss.symphony.messageml.exceptions.InvalidInputException;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

/**
 * An implementation of IDataProvider for test purposes.
 * @author lukasz
 * @since 07/04/17
 */
public class TestDataProvider implements IDataProvider {
  private static final Set<String> STANDARD_URI_SCHEMES = new HashSet<>();
  private UserPresentation user;

  public TestDataProvider() {
    STANDARD_URI_SCHEMES.add("http");
    STANDARD_URI_SCHEMES.add("https");
  }

  @Override
  public IUserPresentation getUserPresentation(String email) throws InvalidInputException {
    if (!email.equalsIgnoreCase(user.getEmail())) {
      throw new InvalidInputException("Failed to lookup user \"" + email + "\"");
    }

    return new UserPresentation(user.getId(), user.getScreenName(), user.getPrettyName(), email);
  }

  @Override
  public IUserPresentation getUserPresentation(Long uid) throws InvalidInputException {
    if (uid == null || user.getId() != uid) {
      throw new InvalidInputException("Failed to lookup user \"" + uid + "\"");
    }

    return new UserPresentation(uid, user.getScreenName(), user.getPrettyName());
  }

  @Override
  public void validateURI(URI uri) throws InvalidInputException {
    if (!STANDARD_URI_SCHEMES.contains(uri.getScheme().toLowerCase())) {
      throw new InvalidInputException(
          "URI scheme \"" + uri.getScheme() + "\" is not supported by the pod.");
    }
  }

  public void setUserPresentation(UserPresentation user) {
    this.user = user;
  }

  public void setUserPresentation(long id, String screenName, String prettyName, String email) {
    this.user = new UserPresentation(id, screenName, prettyName, email);
  }
}
