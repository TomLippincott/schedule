# scheduler - interview management utility for CLSP recruitment weekend

Prospects can contact faculty etc

This code base produces a command-line utility for managing the interview
schedules of the CLSP recruitment weekend.  This involves manipulating
Google spreadsheets, and solving constraint-satisfaction problems based
on faculty preferences, availability, and other considerations.  Under
the hood it employs the Microsoft Z3 theorem prover to find optimal
solutions to problems formulated via the SBV library.  The interactions 
with Google APIs are accomplished with the gogol-* libraries and heavy
use of lenses.

## Quick start

```
$ stack build
$ stack exec -- scheduler
```
