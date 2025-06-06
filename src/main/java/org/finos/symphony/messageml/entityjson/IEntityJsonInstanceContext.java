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

import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * A parser context containing an instance.
 * 
 * @author Bruce Skingle
 *
 */
public interface IEntityJsonInstanceContext extends IEntityJsonContext
{
  /**
   * @return  An object describing the source of the instance. Will not return null.
   */
  Object      getInstanceSource();
  
  /**
   * @return  The instance. Will not return null.
   */
  ObjectNode  getInstanceJsonNode();
  
  /**
   * Add the given instance and return the current object as an IEntityJsonInstanceContext.
   * @param instanceSource    An object describing the source of the instance, typically an
   * instance of java.net.URL or java.io.File.
   * @param instanceJsonNode  The instance.
   * 
   * @return The current object as an IEntityJsonInstanceContext.
   * 
   * @throws NullPointerException if either parameter is null.
   */
  IEntityJsonSchemaContext  withSchema(Object schemaSource, ObjectNode schemaJsonNode);
}
