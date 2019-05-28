# Contributing to messageml-utils
Thanks for your interest in the project! Here is some basic information about how to contribute.
 
# Contributor License Agreement (CLA)
All contributions to [Symphony Software Foundation](https://symphony.foundation/) projects must be made under a [Contributor License Agreement](https://symphonyoss.atlassian.net/wiki/display/FM/Legal+Requirements#LegalRequirements-ContributorLicenseAgreement) that authorizes the Foundation to distribute your code under the Apache License. Contributions must also meet the Foundation's [license and notice requirements](https://symphonyoss.atlassian.net/wiki/display/FM/Legal+Requirements) that must also be met.

_NOTE:_ Commits and pull requests to FINOS repositories will only be accepted from those contributors with an active, executed Individual Contributor License Agreement (ICLA) with FINOS OR who are covered under an existing and active Corporate Contribution License Agreement (CCLA) executed with FINOS. Commits from individuals not covered under an ICLA or CCLA will be flagged and blocked by the FINOS Clabot tool. Please note that some CCLAs require individuals/employees to be explicitly named on the CCLA.
 
Pull requests (PRs) submitted to the project cannot be accepted until you have a CLA in place with the Foundation.

*Need an ICLA? Unsure if you are covered under an existing CCLA? Email [help@finos.org](mailto:help@finos.org)*
 
# Contributing Issues
 
## Prerequisites
 
* [ ] Have you searched for duplicate issues?  A simple search for exception error messages or a summary of the unexpected behavior should suffice.
* [ ] Are you running the latest release of the project?
 
## Raising an Issue
* Create your issue in the project issue tracker.
* New issues contain two templates in the description: bug report and enhancement request. Please pick the most appropriate for your issue, and delete the other.
  * Please also tag the new issue with either "Bug" or "Enhancement".
* Please use [Markdown formatting](https://help.github.com/categories/writing-on-github/)
liberally to assist in readability.
  * [Code fences](https://help.github.com/articles/creating-and-highlighting-code-blocks/) for exception stack traces and log entries, for example, massively improve readability.
 
# Contributing Pull Requests (Code & Docs)
To make review of PRs easier, please:
 
 * Please make sure your PRs will merge cleanly - PRs that don't are unlikely to be accepted.
 * For code contributions, follow the general structure of the existing code.
 * For documentation contributions, follow the general structure, language, and tone of the existing docs.
 * Keep PRs small and cohesive - if you have multiple contributions, please submit them as independent PRs.
 * Reference issue #s if your PR has anything to do with an issue (even if it doesn't address it).
 * Minimize non-functional changes (e.g. whitespace).
 * Ensure all new files include a header comment block containing the [Apache License v2.0 and your copyright information](http://www.apache.org/licenses/LICENSE-2.0#apply).
 * If necessary (e.g. due to 3rd party dependency licensing requirements), update the NOTICE file with any required attribution or other notices
 * If your contribution includes source code for any Category B-licensed dependencies, add an appropriate notice to this CONTRIBUTING file
 
## Commit and PR Messages

* **Reference issues, wiki pages, and pull requests liberally!**
* Use the present tense ("Add feature" not "Added feature")
* Use the imperative mood ("Move button left..." not "Moves button left...")
* Limit the first line to 72 characters or less