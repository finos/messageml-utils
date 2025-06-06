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

import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * An error handler which makes  {@link javax.xml.parsers.DocumentBuilder} fail silently instead of printing error
 * messages to stdout.
 * @author lukasz
 * @since 3/24/17
 */
public class NullErrorHandler implements ErrorHandler {
  @Override
  public void warning(SAXParseException exception) throws SAXException {
  }

  @Override
  public void error(SAXParseException exception) throws SAXException {
  }

  @Override
  public void fatalError(SAXParseException exception) throws SAXException {
  }
}
