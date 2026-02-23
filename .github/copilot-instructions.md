# Mathlify 

## Working Effectively

Mill is already setup for you. Currently it is configured via Mill's new YAML based configuration. 

- Build: A cold build can take up to 1 minute. Stay patient. Subsequent builds will be faster - incremental compilation and caching will speed things up.
- Test: ./mill test
- Format: ./mill mill.scalalib.scalafmt.ScalafmtModule/
- Explore: ./mill resolve _ 


## Folder Structure:

- build.mill.yaml: Mill build configuration file.
- .devcontainer/: Configuration for development container.
- .github/workflows/: GitHub Actions workflows for CI/CD.
- src/: Source code for the project.
- test/: Test code for the project.

## Validation: 

The primary form of validation is via unit testing. Mathlify should ensure that the elements it emits can be found in the dom. 

Tests are writting using scala munit, see the folder structure for locations.

## CI

Uses github actions. The workflow is defined in .github/workflows/copilot-setup-steps.yml. It sets up Java 24, checks out the code, and runs the tests.