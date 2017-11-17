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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;

/**
 * Represents a single Structured Object.
 * 
 * Note that there are multiple levels of validation for EntityJSON.
 * 
 * There is a generic Structured Object schema which all Structured Objects must conform to.
 * 
 * There is an EntityJSON schema which describes a single JSON object where each element
 * is a Structured Object. This is the object which is submitted with a single message and
 * is simply a collection of named objects which the message may refer to.
 * 
 * There are individual schemas for specific Structured Object types, but there is no
 * requirement for such a schema to exist. Message originators are allowed to emit messages
 * containing objects which conform only to the generic Structured Object schema which, aside
 * from the mandatory fields of all Structured objects (type ID and type version) and the 
 * constraint that if an id attribute is present that it must be an array of (idType, idValue) 
 * tuples, may contain any valid JSON.
 * 
 * @author Bruce Skingle
 *
 */
public class StructuredObject
{
  private final EntityJsonContext        context_;
  private final JsonNode                 jsonNode_;
  private final String                   toString_;
  private final String                   type_;
  private final String                   version_;
  private final int                      majorVersion_;
  private final int                      minorVersion_;

  private final List<StructuredObjectId> idList_;
      
  /* package */ StructuredObject(EntityJsonContext context)
  {
    this(context, context.getInstanceJsonNode());
  }
  
  /* package */ StructuredObject(EntityJsonContext context, JsonNode jsonNode)
  {
    context_ = context;
    jsonNode_ = jsonNode;
    
    type_ = jsonNode.get("type").asText();
    version_ = jsonNode.get("version").asText();
    
    String[] parts = version_.split("\\.");
    majorVersion_ = Integer.parseInt(parts[0]);
    minorVersion_ = Integer.parseInt(parts[1]);
    
    StringBuffer s = new StringBuffer("StructuredObject(\"");
    
    s.append(type_);
    s.append(" v");
    s.append(version_);
    s.append("\"");
    
    boolean first = true;
    JsonNode idNode = jsonNode.get("id");
    List<StructuredObjectId>  idList = new ArrayList<>();
    if(idNode instanceof ArrayNode)
    {
      for(JsonNode node : idNode)
      {
        StructuredObjectId id = new StructuredObjectId(node);
        
        if(first)
        {
          first = false;
          s.append(", \"");
          s.append(id.toString());
          s.append("\"");
          
        }
        idList.add(id);
      }
      
    }
    
    idList_ = Collections.unmodifiableList(idList);
    
    s.append(")");
    toString_ = s.toString();
  }

  public EntityJsonContext getContext()
  {
    return context_;
  }

  /**
   * Validate this object against the specific schema for this type from the official repo.
   * 
   * @param parser  A parser to do the validation.
   * @return        A context containing the detailed error report if any.
   * @throws SchemaValidationException  If this object is not valid according to it's schema.
   * @throws NoSchemaException          If there is no official schema for this type.
   * @throws InvalidSchemaException     If the schema exists but is not itself valid or cannot be read.
   */
  public EntityJsonContext validate(EntityJsonParser parser) throws SchemaValidationException, NoSchemaException, InvalidSchemaException
  {
    StringBuffer ubuf = new StringBuffer("https://symphonyosf.github.io/symphony-object/proposed");
    
    for(String part : type_.split("\\."))
    {
      ubuf.append("/");
      ubuf.append(part);
    }
    
    ubuf.append("-v");
    ubuf.append(majorVersion_);
    ubuf.append("_");
    ubuf.append(minorVersion_);
    ubuf.append(".json");
    
    try
    {
      return parser.validate(new URL(ubuf.toString()), jsonNode_);
    }
    catch (MalformedURLException e)
    {
      throw new InvalidSchemaException(null, e);
    }
  }

  public String getType()
  {
    return type_;
  }

  public String getVersion()
  {
    return version_;
  }

  public int getMajorVersion()
  {
    return majorVersion_;
  }

  public int getMinorVersion()
  {
    return minorVersion_;
  }

  public List<StructuredObjectId> getIdList()
  {
    return idList_;
  }

  @Override
  public String toString()
  {
    return toString_;
  }
}
