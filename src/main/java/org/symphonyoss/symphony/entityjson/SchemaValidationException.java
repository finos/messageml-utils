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

/**
 * The instance is not valid according to it's schema.
 * 
 * @author Bruce Skingle
 *
 */
public class SchemaValidationException extends EntityJsonException
{
  private static final long serialVersionUID = 1L;

  public SchemaValidationException(EntityJsonContext context, String message, Throwable cause, boolean enableSuppression,
      boolean writableStackTrace)
  {
    super(context, message, cause, enableSuppression, writableStackTrace);
  }

  public SchemaValidationException(EntityJsonContext context, String message, Throwable cause)
  {
    super(context, message, cause);
  }

  public SchemaValidationException(EntityJsonContext context, String message)
  {
    super(context, message);
  }

  public SchemaValidationException(EntityJsonContext context, Throwable cause)
  {
    super(context, cause);
  }

  public SchemaValidationException(EntityJsonContext context)
  {
    super(context);
  }
}
