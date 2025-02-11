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

package org.finos.symphony.messageml.messagemlutils.util;

import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;
import org.finos.symphony.messageml.messagemlutils.exceptions.ProcessingException;
import org.finos.symphony.messageml.messagemlutils.util.instrument.resolver.InstrumentResolution;
import org.finos.symphony.messageml.messagemlutils.util.instrument.resolver.ResolutionResults;

import java.net.URI;
import java.util.List;

/**
 * Used during message parsing to provide external data.
 *
 * @author bruce.skingle
 * @author lukasz
 * @since 6/7/16
 */
public interface IDataProvider {
  /**
   * Retrieve user information object based on user email
   * @param emailAddress Email of the user whose information to retrieve
   * @return User presentation object
   */
  IUserPresentation getUserPresentation(String emailAddress) throws InvalidInputException;

  /**
   * Retrieve user information object based on UID
   * @param uid UID of the user whose information to retrieve
   * @return User presentation object
   */
  IUserPresentation getUserPresentation(Long uid) throws InvalidInputException;

  /**
   * Check an URI against a whitelist of supported protocols.
   * @param uri the URI to check
   * @throws InvalidInputException thrown if the URI protocol is not supported
   * @throws ProcessingException thrown on a malformed URI or a backend error
   */
  void validateURI(URI uri) throws InvalidInputException, ProcessingException;

  /**
   * Retrieve financial tag information based on list of instrument resolution list
   *
   * @param criteria List of instrument resolution criteria
   * @return Resolution results
   * @throws InvalidInputException
   */
  ResolutionResults getFinTagPresentation(List<InstrumentResolution> criteria)
      throws InvalidInputException;
}
