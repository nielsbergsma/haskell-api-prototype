# Haskell API prototype

## What is this?
This is my research into the world of Haskell. Specicificly how to build modern applications (architectures) with Haskell, functional style. Topics will include, but not limited to:
- restful API
- domain driven design
- CQRS / event sourcing
- specification by example
- and so on ...

This repository is (currently) merely a recording of my progress.

## What's done? and what's next?
- [x] lightweight abstraction on top of WAI, as a micro Restful library
- [x] auto-generate API based op OpenAPI v3 spec
- [x] validate url + query parameters
- [x] validate json body, based on OpenAPI v3 spec
- [x] transform body to transfer objects (for both product and sum types)
- [x] comprehensive validation DSL (json body, url + query parameters)
- [x] strong typed error messages (human + machine interpretable), includes JSON path for Request Body error messages
- [x] JWT authorization / authentication, role based with JWK validation
- [ ] "auto wire" transfer objects to commands
- [ ] command handlers, sagas, process managers
- [ ] aggregates, state, persistence
- [ ] event, event handlers
- [ ] ... more to come

## Use or improve?
Sure, just shoot me a message.