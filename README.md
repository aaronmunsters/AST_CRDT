# CRDT AST

### A conflict-free replicated abstract syntax tree

![Scala CI](https://github.com/aaronmunsters/AST_CRDT/actions/workflows/scala.yml/badge.svg)

---

CRDT AST is an implementation of a conflict-free replicated abstract syntax tree (AST) for a minimal LISP-like language.
It aims to automatically merge the replicated AST that may be edited concurrently on two or more replica's.

It is developed in Scala with compliance for Scala.JS, meaning it can be compiled to run both on the JVM but also on a
JavaScript engine.

## 🛠️ How it works

Any time the local source code is changed, the edits are computed by comparing the old and new AST.
These edits are then transformed into an edit script, which is then shared across the network with other replica's such
that they can apply the same edits in the same order, resulting in all replica's representing the same state.  

## 🧑‍🔧 Usage

The `CRDT_AST_ScalaJS.scala` file demonstrates how to integrate the CRDT in a web application.
This file makes use of the `ConflictFreeReplicatedIntAst` class, which extends the `ConflictFreeReplicatedAst` class
with concrete types for the node identities (being a combination of the replica ID and a newly generated integer) and
for the edit identities (consisting of a lamport clock and the replica id).

This still allows for any desired underlying network structure, but a prototype is provided using the
[PeerJS library](https://peerjs.com).

## 🎬 Demo

Since compilation to JavaScript is supported through ScalaJS, you can try this project in your browser.

Clone the repo, open sbt and run `fastLinkJS` (to compile to JavaScript), after which you can open `index.html` to start
the project.

![Demo Gif](media/ast_crdt_demo.gif)

## 🧪 Tests

**Note:**
*due to an incompatibility issue between ScalaTest for ScalaJS
and IntelliJ the tests should be run from the command line.*

The project comes with a suite of tests, these are present in the following folder:

```
src/main/test/scala
```

And they can be run by issuing the following command in the root of the project:

```
sbt test
```

<embed src="./documentation/test-report/test-report.tex">

## 🗂️ Credits

* Project author: Aäron Munsters
* Computing the AST differences - [Fine-grained and Accurate Source Code Differencing](http://dx.doi.org/10.1145/2642937.2642982)
* Computing the Edit Script - [Change detection in hierarchically structured information](https://doi.org/10.1145/235968.233366)
