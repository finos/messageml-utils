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

import javax.annotation.Nullable;

import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * A parser context containing an instance and a schema.
 * 
 * @author Bruce Skingle
 *
 */
public interface IEntityJsonSchemaContext extends IEntityJsonInstanceContext
{
  /**
   * Set the validation result for this context.
   * 
   * It is not intended that this method will be called from outside the current
   * package, it is declared public only by virture of being specified in an interface.
   * 
   * @param validationResult An object describing the result of validation.
   * 
   * @return  The current object (fluent interface)
   */
  IEntityJsonSchemaContext  withValidationResult(Object validationResult);
  
  /**
   * @return  An object describing the source of the schema. Will not return null.
   */
  Object      getSchemaSource();
  
  /**
   * @return  The schema. Will not return null.
   */
  ObjectNode  getSchemaJsonNode();
  
  /**
   * @return  The validation result, will return null if the context is unvalidated.
   */
  @Nullable Object      getValidationResult();
}
