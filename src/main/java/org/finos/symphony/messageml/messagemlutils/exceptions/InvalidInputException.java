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

package org.finos.symphony.messageml.messagemlutils.exceptions;

/**
 * Exception representing MessageML processing error caused by invalid input data.
 *
 * @author bruce.skingle
 * @author lukasz
 * @since 6/7/16
 */
public class InvalidInputException extends MessageMLException {
  private static final long serialVersionUID = 1L;

  public InvalidInputException(String message) {
    super(message);
  }

  public InvalidInputException(String message, Object... args) {
    super(String.format(message, args));
  }

  public InvalidInputException(String message, Throwable cause) {
    super(message, cause);
  }
}
