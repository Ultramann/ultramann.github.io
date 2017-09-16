---
title: JavaScript
published: August 15, 2017
---

Notes on the programming language JavaScript.

<!--more-->

# JavaScript Notes

## General

* Lines are terminated with a `;`. Yay.
* Camel case is convention.
* Variables are declared with the `var` keyword.
* Zero indexed language.
* Scope Rules: 
    * Variables declared with `var` are visible to all enclosing scopes.
    * Variables declared without `var` are all global.

## Data Types

### Boolean

* `true` and `false`.
* `if (condition) { branch }`.

#### Eq
* Will silently recast type when checking for `==`. e.g. `1 == "1"` => `true`.
* There is a strict equality, `===` that checks both type and value. e.g. `1 === "1"` => `false`.
* Similarly `!=` and `!==` exist for non-strict and strict equality checking, respectively.


### Strings

* Immutable
* Strings can be declared with either double or single quotes.
* Can index into them.
* To get the end of the string you have to use `.length - #`.

### Arrays

* Heterogeneous, mutable.
* Append with `.push()` method, left append with `.unshift()`, remove from end with `.pop()`, remove from beginning with `.shift()`.

## Functions

I guess examples are probably better here:

* Function without arguments: `function myFunc() {...}`
* Function with arguments: `function myFunc(arg1, arg2) {...}`
* Function without name, aka anonymous/lambda, function: `function (arg1, arg2) {...}`
