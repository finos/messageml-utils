[![Licence](https://img.shields.io/badge/licence-Apache%20Licence%20%282.0%29-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
[![Maven Central](https://img.shields.io/maven-central/v/org.symphonyoss.symphony/messageml.svg)](http://search.maven.org/#search%7Cga%7C1%7Ca%3A%22messageml%22)
[![Build Status](https://github.com/symphonyoss/messageml-utils/workflows/Build/badge.svg)](https://github.com/symphonyoss/messageml-utils/actions)
[![Maintainability Rating](https://sonarcloud.io/api/project_badges/measure?project=symphonyoss_messageml-utils&metric=sqale_rating)](https://sonarcloud.io/dashboard?id=symphonyoss_messageml-utils)
[![Sonar](https://sonarcloud.io/api/project_badges/measure?project=symphonyoss_messageml-utils&metric=coverage)](https://sonarcloud.io/dashboard?id=symphonyoss_messageml-utils)

# Introduction

MessageML is a markup language used by the Symphony Agent API for representing messages, including formatting (bold, italic, numbered and unnumbered lists etc.) 
and entity data representing [_structured objects_](https://rest-api.symphony.com/docs/objects). 

The format is intended to allow third parties to create and render rich content messages representing complex objects, 
enabling deep workflow integrations across multiple systems, with Symphony being the central hub.

# Description

Support of Symphony structured objects comprises the ability to parse and render the following formats:

* [MessageML](https://rest-api.symphony.com/docs/messagemlv2), 
a superset of PresentationML, adding a number of convenience tags for more complex or frequently used constructs.
It is translated to PresentationML internally by the Agent API before ingestion in Symphony.
* [PresentationML](https://rest-api.symphony.com/docs/messagemlv2#reading-messageml-messages), 
a strict subset of HTML5 describing the markup which controls the formatting (presentation) of a message. 
PresenttionML is the format used internally by Symphony and returned from message ingestion and retrieval by the Agent API.
* [EntityJSON](https://rest-api.symphony.com/docs/objects#message-and-object-presentation), 
JSON data representing complex financial objects and other structured data. 
EntityJSON elements are referenced in PresentationML and are provided by the message originator 
or generated automatically during the expansion of MessageML convenience tags.
* [Freemarker](http://freemarker.org/),
a templating language providing macros (conditional blocks, iterations, assignments etc.) 
to expand message templates into MessageML messages. Freemarker support is experimental.
* [Markdown](http://daringfireball.net/projects/markdown/syntax),
the legacy text format used by Symphony. 
Markdown parsing and rendering capabilities are provided for compatibility with older versions of Symphony.

More specifically, the MessageML parser can be used to:

* validate the input message as correct MessageML or PresentationML and the input data as correct EntityJSON; 
if the input message contains references to entity data, the validation process verifies if the provided data matches those references 
* expand Freemarker templates into MessageML messages; this includes processing Freemarker macros 
and injecting EntityJSON data referenced by template variables
* construct a document tree representing the message
* serialize the MessageML tree as PresentationML; this step includes generation of EntityJSON data from convenience MessageML tags
* serialize the MessageML tree as Markdown
* parse Markdown input into a MessageML document tree

# Usage

```java
/* Instantiate the parsing context. The "dataProvider" object is used to resolve user mentions and check supplied URLs against a whitelist of supported URI schemes. */
MessageMLContext context = new MessageMLContext(/*IDataProvider*/ dataProvider);

/* Parse the message and entity data */
context.parseMessageML(/*String*/ message, /*String*/ entityJSON, /*String*/ version);

/* Parse a Markdown message into a MessageML document tree */
context.parseMarkdown(/*String*/ markdown, /*JsonNode*/ entities, /*JsonNode*/ media);

/* Get the MessageML document tree */
Element messageML = context.getMessageML();

/* Get the PresenttionML representation of the message */
String presentationML = context.getPresentationML();

/* Get the entity data of the message, including data auto-generated from MessageML convenience tags*/
String entityJSON = context.getEntityJson();

/* Get the Markdown representation of the message */
String markdown = context.getMarkdown();

/* Get the plain text of the message */
String text = context.getText();
```
## Contributing

1. Fork it (<https://github.com/symphonyoss/messageml-utils/fork>)
2. Create your feature branch (`git checkout -b feature/fooBar`)
3. Read our [contribution guidelines](.github/CONTRIBUTING.md) and [Community Code of Conduct](https://www.finos.org/code-of-conduct)
4. Commit your changes (`git commit -am 'Add some fooBar'`)
5. Push to the branch (`git push origin feature/fooBar`)
6. Create a new Pull Request

_NOTE:_ Commits and pull requests to FINOS repositories will only be accepted from those contributors with an active, executed Individual Contributor License Agreement (ICLA) with FINOS OR who are covered under an existing and active Corporate Contribution License Agreement (CCLA) executed with FINOS. Commits from individuals not covered under an ICLA or CCLA will be flagged and blocked by the FINOS Clabot tool. Please note that some CCLAs require individuals/employees to be explicitly named on the CCLA.

*Need an ICLA? Unsure if you are covered under an existing CCLA? Email [help@finos.org](mailto:help@finos.org)*

## License

The code in this repository is distributed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).

Copyright 2016-2019 Symphony LLC
