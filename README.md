# logfire

LogFire is a Scala API (DSL) for rule-based programming,
based on the Rete algorithm. It is specifically designed for
monitoring event streams, for example emitted by a running
program, also referred to as runtime verification.
Using Scala's support for defining DSLs allows to write
rules elegantly as part of Scala programs. This combination 
appears attractive from a practical point of view.
The Rete algorithm has been modified to support events
(a special kind of facts with short lifespan).

© [2011]. California Institute of Technology. 
ALL RIGHTS RESERVED. U.S. Government sponsorship acknowledged.  
Any commercial use must be negotiated with the Office of Technology 
Transfer at the California Institute of Technology. The technical data in 
this document is controlled under the U.S. Export Regulations, release to 
foreign persons may require an export authorization.
