## Weekly Programming Challenge #7

Jamis Buck publishes programming challenges on his blog at 
https://medium.com/@jamis/. This is my submission for week 7. This time it's
all about B+trees. I managed to (almost) complete normal mode, which includes
- A basic B+ tree with configurable arity (children per node).
- The insertion algorithm, allowing you to add a new key/value pair to the tree.
- The search algorithm, allowing you to find the value for a named key.

I also included *Configurable key and value data types* from the list of Hard Mode.

The project is created using Holy Project (https://github.com/yogsototh/holy-project) which is a project generator for Haskell using Cabal.

## Usage
Build: 
```
cabal build
```
Test: 
```
cabal test
```
