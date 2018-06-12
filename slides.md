class: center, middle

# Into to Reason(ML)

[@monad_cat](https://twitter.com/monad_cat)


```bash
npm install -g bs-platform reason-cli
```

---

class: middle

# Syntax

```
type something('a) = Some('a) | None;

let record = {
  foo: number,
  bar: string,
};

=> [2, "hello"]

let betterRecord = {
  bar: string => unit,
  unpack: something(int) => int => int
};

let addNumbers = (a: number, b: number) => {
  a + b;
};

let addTwo = addNumbers(2);

```

---
class: middle

```

let value = "blahblah";

type raw_object = {.
  "count": int
};

let withCount: raw_object = {
  pub count = 1337

};

=> { "count": 1337 };

withCount##count => 1337

let mutableReference: ref(bool) = ref(true)

mutableReference := false;
mutableReference^ => false

mutableReference := true;
mutableReference^ => true
```

???

Show the compact built representation, reason for quick operation.

There is a way to interact with raw JS objects

---
class: middle

# Record converters

BuckleScript provides an underlying mechanism for automatic generation of converters to and from records into javascript objects.

```
[@bs.deriving jsConverter]
type coordinates = {
  x: int,
  y: int
};

let coordinatesToJs: coordinates => {. "x": int, "y": int};
let coordinatesFromJs: {.. "x": int, "y": int} => coordinates;
```
For added type safety you can use the newtype converter
```
[@bs.deriving {jsConverter: newType}]
type coordinates = {
  x: int,
  y: int
};

let coordinatesToJs: coordinates => abs_coordinates;
let coordinatesFromJs: abs_coordinates => coordinates;

```

???

Converters are shallow, there is an open object

---
class: middle

# Null, undefined, option

`Maybe` is `option`

```
type option('a) = None | Some('a);

let foo = None; /* var foo = 0; */
let bar = Some("thing"); /* var bar = ["thing"] */
```
Unfortunately we don't have monadic operators on hand, helper functions are exported by the `Js.Option` module. There are also constructs for interfacing with JS APIs that can return `null` or `undefined`:

```
let theJsValue: Js.Nullable.t(string) = /* the value you've gotten here */
let nullableString: Js.Nullable.t(string) = Js.Nullable.return("hello");
```

And nullable can be automatically converted to `option` at the FFI boundary:
```
type element;
[@bs.val] [@bs.return nullable] [@bs.scope "document"] external getElementById : string => option(element) = "getElementById";
```

---
class: middle

Advantage of this approach is that the conversion to `option` is only applied if the value isn't decructured at the point of calling the function:
```
switch (getElementById("most-important")) {
  | None => Js.log("none");
  | Some(a) => Js.log(a);
};

=>
var match = document.getElementById("most-important");
if (match == null) {
  console.log("none");
} else {
  console.log(match);
}
```

```
let a = getElementById("b");
Js.log(a);

=>
var a = document.getElementById("b");
console.log((a == null) ? 0 : [a]);
```

---
class: middle

# Module system

We would use the `create` function of observable like so

```
let (observer, createResult) = Observable.create();
```

Modules are resolved from capitalized file names, there are no explicit imports we  are used to from other languages.

There can also be multiple modules in one file:
```
module First = {
  let a = 2;
};

module Second = {
  let b = First.a;
}
```
the same in-order reading applies.

```
module Second = {
  open First;
  let b = a;
}
```

---
class: middle

# Pipes

There are three main function application (pipe) operators:
```
#typeof "@@"
external Pervasives.@@: ('a => 'b, 'a) => 'b = "%apply";

#typeof "|>"
external Pervasives.|>: ('a, 'a => 'b) => 'b = "%revapply";

#typeof "|."
Unknown type
```

`|.` is a fast pipe, because the compiler eliminates it at build time:

```
(Js.log) |. List.map (["a", "b"])


=>
List.map((function(prim) {
  console.log(prim);
  return 0;
}), ["a", ["b"]]);
```

???

Unknown type because |. is part of the language (Reason/BuckleScript).

---
class: middle

# Polymorphic variants

Polymorphic variants can exist on their own:
```
`Flakes;

- : [> `Flakes ] = `Flakes
```
and can be used in multiple types:
```
type foo = [`Flakes | `Sandwitch];
type bar = [`Flakes];
```

Equality is also the equality of constructors:
```
type foo = [`One | `Two];
type bar = [`Two | `One];
let id = (arg: foo) => arg;
let test: bar = `One;
id(test);
- : foo = `One;
```
---
