# WIP: domain dsl

This project provides a DSL for writing your domain types
and generate code for any target language (currently kotlin, swift, soon javascript).

It doesn't compile complicated or weird features of any language,
but, instead, use the common objects that gives approximately the same meaning
(structures, classes, data types).

NOTE: Still need a little bit of refactoring and a better design,
but it's good enough for now.

## The idea

Using common lisp and its incredible development environment, we can get a lot of control over
what we want to do, like filtering what should be rendered, if I want to render directly to file,
of if I want to do some copy and paste.

Maybe, one day, we can manipulate the AST of each language directly.

![image](https://github.com/domaindsl/domaindsl/blob/development/extras/stateism.png?raw=true)

## Usage

Declaring a data type:

```lisp
(defclass another-class () ())

(datatype my-type
  ((constructor-of-my-type-a)
   (constructor-of-my-type-b (:class another-class (:name dependency)))))
```

This example will generate the approximate in swift:

```swift
class AnotherClass {}

enum MyType {
  case constructorOfMyTypeA
  case constructorOfMyTypeB(AnotherClass)
}
```

And the approximately in kotlin:

```kotlin
class AnotherClass {}

open class MyType

class ConstructorOfMyTypeA: MyType()

data class ConstructorOfMyTypeB(val dependency: AnotherClass): MyType()
```

Javascript

```javascript
export function AnotherClass {}

export function MyType() {}

export const ConstructorOfMyTypeA = new (function ConstructorOfMyTypeA() {})

export function ConstructorOfMyTypeB(dependency) {
  if (!(this instanceof ConstructorOfMyTypeB)) { return new ConstructorOfMyTypeB(dependency); }

  this.dependency = dependency;
}
```
