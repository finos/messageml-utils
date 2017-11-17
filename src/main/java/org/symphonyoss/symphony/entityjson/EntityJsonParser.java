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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonschema.core.exceptions.ProcessingException;
import com.github.fge.jsonschema.core.load.configuration.LoadingConfiguration;
import com.github.fge.jsonschema.core.load.configuration.LoadingConfigurationBuilder;
import com.github.fge.jsonschema.main.JsonSchemaFactory;
import com.github.fge.jsonschema.main.JsonSchemaFactoryBuilder;

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
  public static final URL BOND_RFQ_SCHEMA_URL           = getURL("/proposed/org/symphonyoss/fin/rfq/request/bond-v0_1.json");
  public static final URL BOND_RFQ_EXAMPLE_URL          = getURL("/example/org/symphonyoss/fin/rfq/request/bond-01.json");
  
  private static URL getURL(String url)
  {
    try
    {
      return new URL("https://symphonyosf.github.io/symphony-object" + url);
    }
    catch (MalformedURLException e)
    {
      throw new RuntimeException("Invalid URL", e);
    }
  }

  private final JsonSchemaFactory factory_;
  
  /* package */ EntityJsonParser(boolean unrestrictedSchemaLoad)
  {
    if(unrestrictedSchemaLoad)
      factory_ = JsonSchemaFactory.byDefault();
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
  
  public EntityJson parseEntityJson(Reader instanceReader) throws SchemaValidationException, InvalidInstanceException
  {
    try
    {
      return new EntityJson(validate(ENTITY_JSON_SCHEMA_URL, instanceReader));
    }
    catch (NoSchemaException | InvalidSchemaException e)
    {
      // In theory this cannot happen
      throw new RuntimeException(e);
    }
  }
  
  public EntityJson parseEntityJson(JsonNode instance) throws SchemaValidationException
  {
    try
    {
      return new EntityJson(validate(ENTITY_JSON_SCHEMA_URL, instance));
    }
    catch (NoSchemaException | InvalidSchemaException e)
    {
      // In theory this cannot happen
      throw new RuntimeException(e);
    }
  }
  
  public StructuredObject parseStructuredObject(URL instanceUrl) throws SchemaValidationException, InvalidInstanceException
  {
    try
    {
      return new StructuredObject(validate(ENTITY_JSON_SCHEMA_URL, instanceUrl));
    }
    catch (NoSchemaException | InvalidSchemaException e)
    {
      // In theory this cannot happen
      throw new RuntimeException(e);
    }
  }
  
  public StructuredObject parseStructuredObject(Reader instanceReader) throws SchemaValidationException, InvalidInstanceException
  {
    try
    {
      return new StructuredObject(validate(ENTITY_JSON_SCHEMA_URL, instanceReader));
    }
    catch (NoSchemaException | InvalidSchemaException e)
    {
      // In theory this cannot happen
      throw new RuntimeException(e);
    }
  }
  
  public StructuredObject parseStructuredObject(JsonNode instance) throws SchemaValidationException
  {
    try
    {
      return new StructuredObject(validate(ENTITY_JSON_SCHEMA_URL, instance));
    }
    catch (NoSchemaException | InvalidSchemaException e)
    {
      // In theory this cannot happen
      throw new RuntimeException(e);
    }
  }
  
  public EntityJsonContext validate(JsonNode schema, JsonNode instance) throws SchemaValidationException
  {
    return validate(new EntityJsonContext()
        .withSchema(schema)
        .withInstance(instance));
  }
  
  public EntityJsonContext validate(URL schemaUrl, JsonNode instance) throws SchemaValidationException, NoSchemaException, InvalidSchemaException
  {
    EntityJsonContext context = new EntityJsonContext()
        .withInstance(instance);
    
    return validate(context.withSchema(getSchemaJsonNode(context, schemaUrl)));
  }
  
  public EntityJsonContext validate(URL schemaUrl, URL instanceUrl) throws SchemaValidationException, InvalidInstanceException, NoSchemaException, InvalidSchemaException
  {
    EntityJsonContext context = new EntityJsonContext();
    
    return validate(context
        .withInstance(getInstanceJsonNode(context, instanceUrl))
        .withSchema(getSchemaJsonNode(context, schemaUrl)));
  }
  
  public EntityJsonContext validate(URL schemaUrl, Reader in) throws SchemaValidationException, InvalidInstanceException, NoSchemaException, InvalidSchemaException
  {
    EntityJsonContext context = new EntityJsonContext();
    
    return validate(context
        .withInstance(getInstanceJsonNode(context, in))
        .withSchema(getSchemaJsonNode(context, schemaUrl)));
  }
  
  private EntityJsonContext validate(EntityJsonContext context) throws SchemaValidationException
  {  
    try
    {
      return context.withProcessingReport(factory_.getJsonSchema(context.getSchemaJsonNode())
          .validate(context.getInstanceJsonNode(), true));
    }
    catch(ProcessingException e)
    {
      throw new SchemaValidationException(context, e);
    }
  }
  
  private JsonNode getSchemaJsonNode(EntityJsonContext context, URL url) throws NoSchemaException, InvalidSchemaException
  {
    try(Reader in = getSchemaReader(context, url))
    {
      ObjectMapper mapper = new ObjectMapper();
      return mapper.readTree(in);
    }
    catch (IOException e)
    {
      throw new InvalidSchemaException(context, e);
    }
  }
  
  private Reader getSchemaReader(EntityJsonContext context, URL url) throws NoSchemaException, InvalidSchemaException
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
  
  private JsonNode getInstanceJsonNode(EntityJsonContext context, URL url) throws InvalidInstanceException
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
  
  private JsonNode getInstanceJsonNode(EntityJsonContext context, Reader in) throws InvalidInstanceException
  {
    try
    {
      ObjectMapper mapper = new ObjectMapper();
      return mapper.readTree(in);
    }
    catch (IOException e)
    {
      throw new InvalidInstanceException(context, e);
    }
  }
  
  private Reader getInstanceReader(EntityJsonContext context, URL url) throws InvalidInstanceException
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
