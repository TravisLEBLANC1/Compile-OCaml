# Upgrades:
- nested pattern
- check for duplicate variable in pattern matching
- unify 3 part of the project
(- add putint as an instruction)

# known problems:

## unused variable
if we have a variable that is never alive, it's not given a register
which means that if we set it to a value, the compiler assume it's a global, and it's never found on the mars execution