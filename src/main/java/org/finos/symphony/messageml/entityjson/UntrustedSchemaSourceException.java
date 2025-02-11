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

package org.finos.symphony.messageml.entityjson;

/**
 * The instance cannot be validated because it refers to a schema from an untrusted source.
 * 
 * The EntityJSON validator can operate in an unconstrained mode where it will download
 * referenced schemas from any http or https URL, or a safe mode where it will only
 * download from known hosts. This is because a malicious user could use the schema
 * download as an attack vector.
 * 
 * This exception indicates that a reference exists, either in the instance document itself
 * or in a schema referenced from it directly or indirectly, to a URL from a host which is
 * not one of the official registries of EntityJSON schemas.  
 * 
 * @author Bruce Skingle
 *
 */
public class UntrustedSchemaSourceException extends EntityJsonException
{
  private static final long serialVersionUID = 1L;

  public UntrustedSchemaSourceException(IEntityJsonContext context, String message, Throwable cause, boolean enableSuppression,
      boolean writableStackTrace)
  {
    super(context, message, cause, enableSuppression, writableStackTrace);
  }

  public UntrustedSchemaSourceException(IEntityJsonContext context, String message, Throwable cause)
  {
    super(context, message, cause);
  }

  public UntrustedSchemaSourceException(IEntityJsonContext context, String message)
  {
    super(context, message);
  }

  public UntrustedSchemaSourceException(IEntityJsonContext context, Throwable cause)
  {
    super(context, cause);
  }

  public UntrustedSchemaSourceException(IEntityJsonContext context)
  {
    super(context);
  }
}
