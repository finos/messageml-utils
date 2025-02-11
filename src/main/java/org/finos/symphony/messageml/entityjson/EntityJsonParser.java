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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.github.fge.jsonschema.core.exceptions.ProcessingException;
import com.github.fge.jsonschema.core.load.configuration.LoadingConfiguration;
import com.github.fge.jsonschema.core.load.configuration.LoadingConfigurationBuilder;
import com.github.fge.jsonschema.core.report.ProcessingReport;
import com.github.fge.jsonschema.main.JsonSchemaFactory;
import com.github.fge.jsonschema.main.JsonSchemaFactoryBuilder;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;

/**
 * A parser for EntityJSON and StructuredObject.
 * 
 * Obtain via EntityJsonParserBuilder.
 * 
 * @author Bruce Skingle
 *
 */
public class EntityJsonParser
{
  public static final URL JSON_SCHEMA_SCHEMA_URL        = getURL("/schema/org/json-schema/draft-04/schema.json");
  public static final URL ENTITY_JSON_SCHEMA_URL        = getURL("/schema/entity-json-v0_1.json");
  public static final URL ENTITY_JSON_EXAMPLE_URL       = getURL("/example/entity-json-01.json");
  public static final URL STRUCTURED_OBJECT_SCHEMA_URL  = getURL("/schema/structured-object-v0_1.json");
  /* package */ static final URL BOND_RFQ_SCHEMA_URL    = getURL("/proposed/org/symphonyoss/fin/rfq/request/bond-v0_1.json");
  /* package */ static final URL BOND_RFQ_EXAMPLE_URL   = getURL("/example/org/symphonyoss/fin/rfq/request/bond-01.json");
  
  private static URL getURL(String url)
  {
    try
    {
      return new URL("https://schemas.oss.symphony.com" + url);
    }
    catch (MalformedURLException e)
    {
      throw new RuntimeException("Invalid URL", e);
    }
  }

  private final JsonSchemaFactory factory_;
  
  /* package */ EntityJsonParser(boolean unrestrictedSchemaLoad)
  {
    if(unrestrictedSchemaLoad) {
      factory_ = JsonSchemaFactory.byDefault();
    }
    else
    {
      JsonSchemaFactoryBuilder builder = JsonSchemaFactory.newBuilder();
      
      LoadingConfigurationBuilder loadingCfgBuilder = LoadingConfiguration.newBuilder();

      loadingCfgBuilder.addScheme("http", EntityJsonURIDownloader.getInstance());
      loadingCfgBuilder.addScheme("https", EntityJsonURIDownloader.getInstance());
      
      LoadingConfiguration loadingCfg = loadingCfgBuilder.freeze();
      
      builder.setLoadingConfiguration(loadingCfg);
      
      factory_ = builder.freeze();
    }
  }
  
  /**
   * Parse an EntityJSON instance from the given URL.
   * 
   * Callers may prefer to catch EntityJSONException and treat all failures in the same way.
   * 
   * @param instanceUrl A URL pointing to the JSON representation of an EntityJSON instance.
   * 
   * @return  An EntityJSON instance.
   * 
   * @throws SchemaValidationException  If the given instance does not meet the general EntityJSON schema.
   * @throws InvalidInstanceException   If the given instance is structurally invalid.
   */
  public EntityJson parseEntityJson(URL instanceUrl) throws SchemaValidationException, InvalidInstanceException
  {
    try
    {
      return new EntityJson(validate(ENTITY_JSON_SCHEMA_URL, instanceUrl));
    }
    catch (NoSchemaException | InvalidSchemaException e)
    {
      // In theory this cannot happen
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Parse an EntityJSON instance from the given URL.
   * 
   * Callers may prefer to catch EntityJSONException and treat all failures in the same way.
   * 
   * @param instanceSource  An object describing the source of the instance, typically an instance
   * of java.net.URL or java.io.File.
   * @param instanceReader  A Reader containing the JSON representation of an EntityJSON instance.
   * 
   * @return  An EntityJSON instance.
   * 
   * @throws SchemaValidationException  If the given instance does not meet the general EntityJSON schema.
   * @throws InvalidInstanceException   If the given instance is structurally invalid.
   */
  public EntityJson parseEntityJson(Object instanceSource, Reader instanceReader) throws SchemaValidationException, InvalidInstanceException
  {
    try
    {
      return new EntityJson(validate(ENTITY_JSON_SCHEMA_URL, instanceSource, instanceReader));
    }
    catch (NoSchemaException | InvalidSchemaException e)
    {
      // In theory this cannot happen
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Parse an EntityJSON instance from the given URL.
   * 
   * Callers may prefer to catch EntityJSONException and treat all failures in the same way.
   * 
   * @param instanceSource  An object describing the source of the instance, typically an instance
   * of java.net.URL or java.io.File.
   * @param instance        A JSON ObjectNode containing the JSON representation of an EntityJSON instance.
   * 
   * @return  An EntityJSON instance.
   * 
   * @throws SchemaValidationException  If the given instance does not meet the general EntityJSON schema.
   * @throws InvalidInstanceException   If the given instance is structurally invalid.
   */
  public EntityJson parseEntityJson(Object instanceSource, ObjectNode instance) throws SchemaValidationException, InvalidInstanceException
  {
    try
    {
      return new EntityJson(validate(ENTITY_JSON_SCHEMA_URL, instanceSource, instance));
    }
    catch (NoSchemaException | InvalidSchemaException e)
    {
      // In theory this cannot happen
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Parse a single StructuredObject instance from the given URL.
   * 
   * Callers may prefer to catch EntityJSONException and treat all failures in the same way.
   * 
   * @param instanceUrl A URL pointing to the JSON representation of a StructuredObject instance.
   * 
   * @return  A StructuredObject instance.
   * 
   * @throws SchemaValidationException  If the given instance does not meet the general EntityJSON schema.
   * @throws InvalidInstanceException   If the given instance is structurally invalid.
   */
  public StructuredObject parseStructuredObject(URL instanceUrl) throws SchemaValidationException, InvalidInstanceException
  {
    try
    {
      return new StructuredObject(validate(STRUCTURED_OBJECT_SCHEMA_URL, instanceUrl));
    }
    catch (NoSchemaException | InvalidSchemaException e)
    {
      // In theory this cannot happen
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Parse a single StructuredObject instance from the given URL.
   * 
   * Callers may prefer to catch EntityJSONException and treat all failures in the same way.
   * 
   * @param instanceSource  An object describing the source of the instance, typically an instance
   * of java.net.URL or java.io.File.
   * @param instanceReader  A Reader containing the JSON representation of a single StructuredObject instance.
   * 
   * @return  A StructuredObject instance.
   * 
   * @throws SchemaValidationException  If the given instance does not meet the general EntityJSON schema.
   * @throws InvalidInstanceException   If the given instance is structurally invalid.
   */
  public StructuredObject parseStructuredObject(Object instanceSource, Reader instanceReader) throws SchemaValidationException, InvalidInstanceException
  {
    try
    {
      return new StructuredObject(validate(STRUCTURED_OBJECT_SCHEMA_URL, instanceSource, instanceReader));
    }
    catch (NoSchemaException | InvalidSchemaException e)
    {
      // In theory this cannot happen
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Parse a single StructuredObject instance from the given URL.
   * 
   * Callers may prefer to catch EntityJSONException and treat all failures in the same way.
   * 
   * @param instanceSource  An object describing the source of the instance, typically an instance
   * of java.net.URL or java.io.File.
   * @param instance        A JSON ObjectNode containing the JSON representation of a single StructuredObject instance.
   * 
   * @return  An StructuredObject instance.
   * 
   * @throws SchemaValidationException  If the given instance does not meet the general EntityJSON schema.
   * @throws InvalidInstanceException   If the given instance is structurally invalid.
   */
  public StructuredObject parseStructuredObject(Object instanceSource, ObjectNode instance) throws SchemaValidationException
  {
    try
    {
      return new StructuredObject(validate(STRUCTURED_OBJECT_SCHEMA_URL, instanceSource, instance));
    }
    catch (NoSchemaException | InvalidSchemaException e)
    {
      // In theory this cannot happen
      throw new RuntimeException(e);
    }
  }
  
  /* package */ IEntityJsonSchemaContext validate(Object schemaSource, ObjectNode schema, Object instanceSource, ObjectNode instance) throws SchemaValidationException
  {
    return validate(EntityJsonContext.newInstance()
        .withInstance(instanceSource, instance)
        .withSchema(schemaSource, schema)
        );
  }
  
  /* package */ IEntityJsonSchemaContext validate(URL schemaUrl, Object instanceSource, ObjectNode instance) throws SchemaValidationException, NoSchemaException, InvalidSchemaException
  {
    IEntityJsonInstanceContext context = EntityJsonContext.newInstance()
        .withInstance(instanceSource, instance);
    
    return validate(context.withSchema(schemaUrl, getSchemaJsonNode(context, schemaUrl)));
  }
  
  /* package */ IEntityJsonSchemaContext validate(URL schemaUrl, URL instanceUrl) throws SchemaValidationException, InvalidInstanceException, NoSchemaException, InvalidSchemaException
  {
    IEntityJsonContext context = EntityJsonContext.newInstance();
    
    return validate(context
        .withInstance(instanceUrl, getInstanceJsonNode(context, instanceUrl))
        .withSchema(schemaUrl, getSchemaJsonNode(context, schemaUrl)));
  }
  
  /* package */ IEntityJsonSchemaContext validate(URL schemaUrl, Object instanceSource, Reader in) throws SchemaValidationException, InvalidInstanceException, NoSchemaException, InvalidSchemaException
  {
    IEntityJsonContext context = EntityJsonContext.newInstance();
    
    return validate(context
        .withInstance(instanceSource, getInstanceJsonNode(context, in))
        .withSchema(schemaUrl, getSchemaJsonNode(context, schemaUrl)));
  }
  
  private IEntityJsonSchemaContext validate(IEntityJsonSchemaContext context) throws SchemaValidationException
  {  
    try
    {
      ProcessingReport validationResult = factory_.getJsonSchema(context.getSchemaJsonNode())
          .validate(context.getInstanceJsonNode(), true);
      
      context.withValidationResult(validationResult);
      
      if(!validationResult.isSuccess())
      {
        throw new SchemaValidationException(context);
      }
      
      return context.withValidationResult(validationResult);
    }
    catch(ProcessingException e)
    {
      throw new SchemaValidationException(context, e);
    }
  }
  
  private ObjectNode getSchemaJsonNode(IEntityJsonContext context, URL url) throws NoSchemaException, InvalidSchemaException
  {
    try(Reader in = getSchemaReader(context, url))
    {
      ObjectMapper mapper = new ObjectMapper();
      return (ObjectNode)mapper.readTree(in);
    }
    catch (IOException | ClassCastException e)
    {
      throw new InvalidSchemaException(context, e);
    }
  }
  
  private Reader getSchemaReader(IEntityJsonContext context, URL url) throws NoSchemaException, InvalidSchemaException
  {
    try
    {
      return new InputStreamReader(url.openStream(), StandardCharsets.UTF_8);
    }
    catch (FileNotFoundException e)
    {
      throw new NoSchemaException(context, e);
    }
    catch (IOException e)
    {
      throw new InvalidSchemaException(context, e);
    }
  }
  
  private ObjectNode getInstanceJsonNode(IEntityJsonContext context, URL url) throws InvalidInstanceException
  {
    try(Reader in = getInstanceReader(context, url))
    {
      return getInstanceJsonNode(context, in);
    }
    catch (IOException e)
    {
      throw new InvalidInstanceException(context, e);
    }
  }
  
  private ObjectNode getInstanceJsonNode(IEntityJsonContext context, Reader in) throws InvalidInstanceException
  {
    try
    {
      ObjectMapper mapper = new ObjectMapper();
      return (ObjectNode)mapper.readTree(in);
    }
    catch (IOException | ClassCastException e)
    {
      throw new InvalidInstanceException(context, e);
    }
  }
  
  private Reader getInstanceReader(IEntityJsonContext context, URL url) throws InvalidInstanceException
  {
    try
    {
      return new InputStreamReader(url.openStream(), StandardCharsets.UTF_8);
    }
    catch (IOException e)
    {
      throw new InvalidInstanceException(context, e);
    }
  }
}
