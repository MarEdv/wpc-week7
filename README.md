## Weekly Programming Challenge #7

Jamis Buck publishes programming challenges on his blog at 
https://medium.com/@jamis/. This is my submission for week 7. This time it's
all about B+trees. I managed to (almost) complete normal mode, which includes
- A basic B+ tree with configurable arity (children per node).
- The insertion algorithm, allowing you to add a new key/value pair to the tree.
- The search algorithm, allowing you to find the value for a named key.

I also included *Configurable key and value data types* from the list of Hard Mode.

The project is created using Holy Project (https://github.com/yogsototh/holy-project) which is a project generator for Haskell using Cabal.

## Build, run and test
There is no executable, only the package and unit tests

Build: 
```
cabal build
```
Test: 
```
cabal test
```
## Usage
The tree consists of the type
```(Ord key) => BplusTree key value```
with one type parameter for keys and one for values.
The available construcors are
```
    Node {
      arity :: Int
      , keys :: [key]
      , children :: [BplusTree key value]
      , sibling :: (Maybe (BplusTree key value))
    }
```
and
```
    Leaf {
       arity :: Int
       , keyValues :: [(key, value)]
       , sibling :: (Maybe (BplusTree key value))
    }
```
Each constructor takes the arity of the tree. This is needed when creating the
root node. Later this is handled by the insert function.

Insert a mapping from key 4 to value '4' in an empty tree with an arity of 3, like this:
```insert 4 '4' (createEmptyRoot 3)```

Finding a Leaf that contains the mapping for a key like this:
```searchNode 4 tree```
where tree is a BplusTree.