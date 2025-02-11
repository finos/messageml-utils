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

import java.util.Objects;

import javax.annotation.Nullable;

/**
 * Represents the context of a parsing and validation operation.
 * 
 * Returned from validate() methods and contained within various exception classes.
 * 
 * All access to this class should be via the interfaces IEntityJsonContext and its sub-interfaces
 * to ensure null correctness.
 * 
 * @author Bruce Skingle
 *
 */
/* package */ class EntityJsonContext implements IEntityJsonSchemaContext
{
  private Object     instanceSource_;
  private ObjectNode instanceJsonNode_;
  private Object     schemaSource_;
  private ObjectNode schemaJsonNode_;
  private Object     validationResult_;
      
  private EntityJsonContext()
  {}
  
  /**
   * Create a new instance of IEntityJsonContext
   * 
   * @return A new instance of IEntityJsonContext
   */
  public static IEntityJsonContext newInstance()
  {
    return (IEntityJsonContext)new EntityJsonContext();
  }
  
  @Override
  public IEntityJsonSchemaContext  withSchema(Object schemaSource, ObjectNode schemaJsonNode)
  {
    Objects.requireNonNull(schemaSource, "schemaSource may not be null.");
    Objects.requireNonNull(schemaJsonNode, "schemaJsonNode may not be null.");
    
    schemaSource_ = schemaSource;
    schemaJsonNode_ = schemaJsonNode;
    return (IEntityJsonSchemaContext)this;
  }

  @Override
  public IEntityJsonInstanceContext  withInstance(Object instanceSource, ObjectNode instanceJsonNode)
  {
    Objects.requireNonNull(instanceSource, "instanceSource may not be null.");
    Objects.requireNonNull(instanceJsonNode, "instanceJsonNode may not be null.");
    
    instanceSource_ = instanceSource;
    instanceJsonNode_ = instanceJsonNode;
    return (IEntityJsonInstanceContext)this;
  }

  @Override
  public ObjectNode getSchemaJsonNode()
  {
    return schemaJsonNode_;
  }

  @Override
  public Object getInstanceSource()
  {
    return instanceSource_;
  }

  @Override
  public Object getSchemaSource()
  {
    return schemaSource_;
  }

  @Override
  public @Nullable Object getValidationResult()
  {
    return validationResult_;
  }

  @Override
  public ObjectNode getInstanceJsonNode()
  {
    return instanceJsonNode_;
  }

  @Override
  public String toString()
  {
    StringBuffer s = new StringBuffer("EntityJsonContext");
    
    if(instanceSource_ != null)
    {
      s.append(" instanceSource=\"");
      s.append(instanceSource_);
      s.append('"');
    }
    
    if(schemaSource_ != null)
    {
      s.append(" schemaSource=\"");
      s.append(schemaSource_);
      s.append('"');
    }
    
    if(validationResult_ != null)
    {
      s.append(" validationResult=\"");
      s.append(validationResult_);
      s.append('"');
    }
      
    return s.toString();
  }

  @Override
  public IEntityJsonSchemaContext  withValidationResult(Object validationResult)
  {
    validationResult_ = validationResult;
    return (IEntityJsonSchemaContext)this;
  }
}
