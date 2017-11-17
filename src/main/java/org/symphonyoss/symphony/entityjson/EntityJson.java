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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Represents an EntityJSON document which is an object containing one or more
 * StructuredObjects.
 * 
 * @author Bruce Skingle
 *
 */
public class EntityJson implements Iterable<StructuredObject>
{
  private final EntityJsonContext             context_;
  private final List<StructuredObject>        children_;
  private final Map<String, StructuredObject> childMap_;
  
  public EntityJson(EntityJsonContext context)
  {
    context_ = context;
    
    JsonNode jsonNode = context_.getInstanceJsonNode();
    
    List<StructuredObject>  children = new ArrayList<>();
    Map<String, StructuredObject>  childMap = new HashMap<>();
    
    Iterator<Entry<String, JsonNode>> it = ((ObjectNode)jsonNode).fields();
    
    while(it.hasNext())
    {
      Entry<String, JsonNode> entry = it.next();
      
      StructuredObject obj = new StructuredObject(context_, entry.getValue());
      
      children.add(obj);
      childMap.put(entry.getKey(), obj);
    }
    
    children_ = Collections.unmodifiableList(children);
    childMap_ = Collections.unmodifiableMap(childMap);
  }

  public EntityJsonContext getContext()
  {
    return context_;
  }

  public List<StructuredObject> getChildren()
  {
    return children_;
  }

  @Override
  public Iterator<StructuredObject> iterator()
  {
    return children_.iterator();
  }

  public StructuredObject get(String key)
  {
    return childMap_.get(key);
  }

  public int size()
  {
    return children_.size();
  }

  public boolean isEmpty()
  {
    return children_.isEmpty();
  }

  public boolean contains(StructuredObject o)
  {
    return children_.contains(o);
  }

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
