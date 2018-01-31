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

import org.junit.Test;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

/*
 * Test various valid inputs, the validate method throws an exception if the input is invalid so
 * completion of each method without an exception is sufficient to pass.
 */
public class TestValidSchemas
{
  private EntityJsonParser getParser()
  {
    return new EntityJsonParserBuilder().build();
  }

  @Test
  public void testJsonSchema() throws EntityJsonException
  {
    getParser().validate(EntityJsonParser.JSON_SCHEMA_SCHEMA_URL, EntityJsonParser.JSON_SCHEMA_SCHEMA_URL);
  }

  @Test
  public void testEntityJsonSchema() throws EntityJsonException
  {
    getParser().validate(EntityJsonParser.JSON_SCHEMA_SCHEMA_URL, EntityJsonParser.ENTITY_JSON_SCHEMA_URL);
  }
  
  @Test
  public void testEntityJson() throws EntityJsonException
  {
    getParser().validate(EntityJsonParser.ENTITY_JSON_SCHEMA_URL, EntityJsonParser.ENTITY_JSON_EXAMPLE_URL);
  }
  

  @Test
  public void testStructuredObject() throws EntityJsonException
  {
    getParser().validate(EntityJsonParser.STRUCTURED_OBJECT_SCHEMA_URL, EntityJsonParser.BOND_RFQ_EXAMPLE_URL);
  }
  

  @Test
  public void testBondRfqSchema() throws EntityJsonException
  {
    getParser().validate(EntityJsonParser.JSON_SCHEMA_SCHEMA_URL, EntityJsonParser.BOND_RFQ_SCHEMA_URL);
  }
  
  @Test
  public void testBondRfq() throws EntityJsonException
  {
    getParser().validate(EntityJsonParser.BOND_RFQ_SCHEMA_URL, EntityJsonParser.BOND_RFQ_EXAMPLE_URL);
  }
  
  @Test
  public void testBondRfqObject() throws EntityJsonException, IOException
  {
    StructuredObject obj = getParser().parseStructuredObject(EntityJsonParser.BOND_RFQ_EXAMPLE_URL);
    
    System.err.println("obj=" + obj);
    
    IEntityJsonSchemaContext context = obj.validate(getParser());
    
    System.err.println("validate=" + context);
  }
  
  @Test
  public void testSingleJira() throws SchemaValidationException, InvalidInstanceException, IOException
  {
    test("payloads/single_jira_ticket.json");
  }
  
  @Test
  public void testExpandedJira() throws SchemaValidationException, InvalidInstanceException, IOException
  {
    test("payloads/expanded_single_jira_ticket.json");
  }
  
  private void test(String name) throws SchemaValidationException, InvalidInstanceException, IOException
  {
    EntityJson entityJson = getParser().parseEntityJson("Unit Test Hard coded value", getPayload(name));
    
    System.err.println("entityJson=" + entityJson);
    
    for(StructuredObject obj : entityJson)
      System.err.println("obj=" + obj);
  }
  
  private Reader getPayload(String filename) throws IOException
  {
    return new InputStreamReader(
        getClass().getClassLoader().getResourceAsStream(filename)
        );
  }
}
