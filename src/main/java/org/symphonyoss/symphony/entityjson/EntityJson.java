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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.Nullable;

/**
 * Represents an EntityJSON document which is an object containing one or more
 * StructuredObjects.
 * 
 * @author Bruce Skingle
 *
 */
public class EntityJson implements Iterable<StructuredObject>
{
  private final IEntityJsonInstanceContext    context_;
  private final List<StructuredObject>        children_;
  private final Map<String, StructuredObject> childMap_;
  
  /**
   * Construct an EntityJson from the given context.
   * 
   * Each object within the EntityJSON structure is parsed and validated against the
   * general StructuredObject schema. Callers who wish to validate individual objects
   * against their specific schema need to call validate() on each object in turn,
   * using this object as an Iterable<StructuredObject> if desired.
   * 
   * Although this class and its constructor are public, it is not envisaged that 
   * clients will call it directly, rather one would expect instances to be 
   * obtained via <code>EntityJsonParser.parseStructuredObject(...)</code>
   * 
   * @param context A parser context.
   * 
   * @throws InvalidInstanceException If the EntityJSON structure contains elements whch
   * are not valid STructuredObjects.
   */
  public EntityJson(IEntityJsonInstanceContext context) throws InvalidInstanceException
  {
    context_ = context;
    
    JsonNode jsonNode = context_.getInstanceJsonNode();
    
    List<StructuredObject>  children = new ArrayList<>();
    Map<String, StructuredObject>  childMap = new HashMap<>();
    
    Iterator<Entry<String, JsonNode>> it = jsonNode.fields();
    
    while(it.hasNext())
    {
      Entry<String, JsonNode> entry = it.next();
      
      ObjectNode objectInstance;
      
      try
      {
        objectInstance = (ObjectNode) entry.getValue();
      }
      catch(ClassCastException e)
      {
        throw new InvalidInstanceException(context, e);
      }
      StructuredObject obj = new StructuredObject(context_, context_.getInstanceSource(), objectInstance);
      
      children.add(obj);
      childMap.put(entry.getKey(), obj);
    }
    
    children_ = Collections.unmodifiableList(children);
    childMap_ = Collections.unmodifiableMap(childMap);
  }

  /**
   * Return the parse context from which this object was created, which includes error reports
   * if the input was invalid in some way.
   * 
   * @return The parse context from which this object was created.
   */
  public IEntityJsonInstanceContext getContext()
  {
    return context_;
  }

  /**
   * Return the StructuredObjects contained within this object as a list.
   * 
   * @return The StructuredObjects contained within this object.
   */
  public List<StructuredObject> getChildren()
  {
    return children_;
  }

  @Override
  public Iterator<StructuredObject> iterator()
  {
    return children_.iterator();
  }

  /**
   * Return the StructuredObject for the given name, or null.
   * 
   * @param name  The name of the required object.
   * 
   * @return The required child object, or null if the given name does not exist.
   */
  public @Nullable StructuredObject get(String name)
  {
    return childMap_.get(name);
  }

  /**
   * As per the method of the same name in java.util.List<StructuredObject>
   */
  public int size()
  {
    return children_.size();
  }

  /**
   * As per the method of the same name in java.util.List<StructuredObject>
   */
  public boolean isEmpty()
  {
    return children_.isEmpty();
  }

  /**
   * As per the method of the same name in java.util.List<StructuredObject>
   */
  public boolean contains(StructuredObject o)
  {
    return children_.contains(o);
  }

  /**
   * As per the method of the same name in java.util.List<StructuredObject>
   */
  public StructuredObject get(int index)
  {
    return children_.get(index);
  }

  @Override
  public String toString()
  {
    return "EntityJson" + childMap_;
  }
}
