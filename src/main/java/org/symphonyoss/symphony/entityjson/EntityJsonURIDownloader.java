/*
 *
 *
 * Copyright 2017 Symphony Communication Services, LLC.
 *
 * Licensed to The Symphony Software Foundation (SSF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The SSF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.symphonyoss.symphony.entityjson;

import com.github.fge.jsonschema.core.load.download.URIDownloader;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;

/**
 * URIDownloader which restricts schema downloads to known hosts.
 * 
 * @author Bruce Skingle
 *
 */
public class EntityJsonURIDownloader implements URIDownloader
{
  private static final URIDownloader INSTANCE = new EntityJsonURIDownloader();

  private EntityJsonURIDownloader()
  {
  }

  /**
   * Singleton getter.
   * 
   * This is merely a performance optimisation, this class does not rely on there being only
   * a single instance.
   * 
   * @return The single instance.
   */
  public static URIDownloader getInstance()
  {
    return INSTANCE;
  }

  @Override
  public InputStream fetch(final URI source) throws IOException
  {
    switch(source.getHost())
    {
      case "schemas.oss.symphony.com":
      case "symphonyosf.github.io":
      case "object.symphonyoss.org":
      case "object.symphony.com":
        return source.toURL().openStream();
    }
    
    throw new IOException("URI is from unrecognized host - aborted.");
  }
}