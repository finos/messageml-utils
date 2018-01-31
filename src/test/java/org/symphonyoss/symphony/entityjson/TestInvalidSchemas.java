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
import java.io.StringReader;

/*
 * Test various invalid inputs, the validate method throws an exception if the input is invalid.
 */
public class TestInvalidSchemas
{
  private EntityJsonParser getParser()
  {
    return new EntityJsonParserBuilder().build();
  }
  
  @Test(expected=SchemaValidationException.class)
  public void testInvalidTypeVersion() throws EntityJsonException, IOException
  {
    StructuredObject obj = getParser().parseStructuredObject(
        "Unit Test object with invalid type version.",
        new StringReader("{\n" + 
        "    \"type\":     \"org.symphonyoss.fin.rfq.request.bond\",\n" + 
        "    \"version\":  \"9\",\n" + 
        "    \"id\": [\n" + 
        "      {\n" + 
        "        \"type\": \"com.factset.portware.ticket.id\",\n" + 
        "        \"value\": \"123456789\"\n" + 
        "      }\n" + 
        "    ],\n" + 
        "    \"side\": \"BUY\"\n" + 
        "}"));
    
    obj.validate(getParser());
    
    // N.B not ArrayIndexOutOfBoundsException
  }
 
  @Test(expected=SchemaValidationException.class)
  public void testMissingTypeVersion() throws EntityJsonException, IOException
  {
    StructuredObject obj = getParser().parseStructuredObject(
        "Unit Test object with invalid type version.",
        new StringReader("{\n" + 
        "    \"type\":     \"org.symphonyoss.fin.rfq.request.bond\",\n" + 
        "    \"id\": [\n" + 
        "      {\n" + 
        "        \"type\": \"com.factset.portware.ticket.id\",\n" + 
        "        \"value\": \"123456789\"\n" + 
        "      }\n" + 
        "    ],\n" + 
        "    \"side\": \"BUY\"\n" + 
        "}"));
    
    obj.validate(getParser());
  }
  
  @Test(expected=SchemaValidationException.class)
  public void testMissingTypeId() throws EntityJsonException, IOException
  {
    StructuredObject obj = getParser().parseStructuredObject(
        "Unit Test object with invalid type version.",
        new StringReader("{\n" + 
        "    \"version\":  \"1.2\",\n" + 
        "    \"id\": [\n" + 
        "      {\n" + 
        "        \"type\": \"com.factset.portware.ticket.id\",\n" + 
        "        \"value\": \"123456789\"\n" + 
        "      }\n" + 
        "    ],\n" + 
        "    \"side\": \"BUY\"\n" + 
        "}"));
    
    obj.validate(getParser());
  }
  
  @Test(expected=SchemaValidationException.class)
  public void testObjectIdString() throws EntityJsonException, IOException
  {
    StructuredObject obj = getParser().parseStructuredObject(
        "Unit Test object with invalid type version.",
        new StringReader("{\n" + 
        "    \"type\":     \"org.symphonyoss.fin.rfq.request.bond\",\n" + 
        "    \"version\":  \"1.2\",\n" + 
        "    \"id\": \"com.factset.portware.ticket.id\",\n" + 
        "    \"side\": \"BUY\"\n" + 
        "}"));
    
    obj.validate(getParser());
  }
  
  @Test(expected=SchemaValidationException.class)
  public void testObjectIdObject() throws EntityJsonException, IOException
  {
    StructuredObject obj = getParser().parseStructuredObject(
        "Unit Test object with invalid type version.",
        new StringReader("{\n" + 
        "    \"type\":     \"org.symphonyoss.fin.rfq.request.bond\",\n" + 
        "    \"version\":  \"1.2\",\n" + 
        "    \"id\": {\n" + 
        "      \"type\": \"com.factset.portware.ticket.id\",\n" + 
        "      \"value\": \"123456789\"\n" + 
        "    },\n" + 
        "    \"side\": \"BUY\"\n" + 
        "}"));
    
    obj.validate(getParser());
  }
  
  @Test(expected=SchemaValidationException.class)
  public void testIdExtraAttributes() throws EntityJsonException, IOException
  {
    StructuredObject obj = getParser().parseStructuredObject(
        "Unit Test object with invalid type version.",
        new StringReader("{\n" + 
        "    \"type\":     \"org.symphonyoss.fin.rfq.request.bond\",\n" + 
        "    \"version\":  \"1.2\",\n" + 
        "    \"id\": [\n" + 
        "      {\n" + 
        "        \"type\": \"com.factset.portware.ticket.id\",\n" + 
        "        \"value\": \"123456789\",\n" + 
        "        \"notAllowed\": \"invalid\"\n" + 
        "      }\n" + 
        "    ],\n" + 
        "    \"side\": \"BUY\"\n" + 
        "}"));
    
    obj.validate(getParser());
  }
}
