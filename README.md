
# HEvents: An Event Sourcing Library for Haskell

![](https://travis-ci.org/abailly/hevents.svg?branch=master)

The goal of this library is to make it easy to develop and deploy production grade (micro-)services based on *event sourced* model.
Each such service should be easily constructed and include the following:

* A underlying *business model* expressed as a pure data structures to which can be sent *Commands* and applied *Events*,
* Atomic application of some commands to underlying model,
    * Possibly with wrapping it in remote request,
* Storage of generated events in append-only data source with choice of underlying storage medium:
    * In memory list of events,
    * Local file,
    * Remote server,
* Call to other services as a client,
* Error handling in a consistent way,
* Web interface to the service,
* Logging,
* Metrics and monitoring,
* Testing,
* Packaging,
* ...

The overarching principle is to expose each feature as a mini-DSL and compose them using extensible-effects approach.

