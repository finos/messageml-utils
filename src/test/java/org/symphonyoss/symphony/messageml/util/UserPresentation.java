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

/**
 * Models Symphony user data.
 *
 * @author bruce.skingle
 * @author lukasz
 * @since 6/7/16
 */
public class UserPresentation implements IUserPresentation {
  private final long id;
  private final String screenName;
  private final String prettyName;
  private final String email;

  public UserPresentation(long id, String screenName, String prettyName) {
    this(id, screenName, prettyName, null);
  }

  public UserPresentation(long id, String screenName, String prettyName, String email) {
    this.id = id;
    this.screenName = screenName;
    this.prettyName = prettyName;
    this.email = email;
  }

  @Override
  public long getId() {
    return id;
  }

  @Override
  public String getScreenName() {
    return screenName;
  }

  @Override
  public String getPrettyName() {
    return prettyName;
  }

  @Override
  public String getEmail() {
    return email;
  }

}
