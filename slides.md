class: center, middle

# Into to Reason(ML)

[@monad_cat](https://twitter.com/monad_cat)


---

class: middle

# What is Reason?

- an OCaml compiler frontend that compiles to JS using bucklescript
- JS-like syntax, but with more functional features we know and love
- easy interop with JS

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

# Named parameters

---
class: middle

# Binding into JS libraries

```
type subscription;
type _observer('a, 'b) = {.
  "closed": ref(bool),
  [@bs.meth] "next": 'a => unit,
  [@bs.meth] "error": 'b => unit,
  [@bs.meth] "complete": unit => unit
};

type observer('a, 'b) = {
  closed: ref(bool),
  next: 'a => unit,
  error: 'b => unit,
  complete: unit => unit
};
type observable('a, 'b) = {.
  [@bs.meth] "subscribe": (('a => unit), ('b => unit), (unit => unit)) => subscription
};

[@bs.scope "Observable"][@bs.module "@reactivex/rxjs/dist/cjs/Observable"][@bs.val] external createObservable: (_observer('a, 'b) => unit) => observable('a, 'b) = "create";

type create('a, 'b) = (unit) => (observer('a, 'b), observable('a, 'b));

let create: create('a, 'b) = () => {
  let realObservers: ref(list(_observer('a, 'b))) = ref([]);
  let observer: observer('a, 'b) = {
    closed: ref(false),
    next: (el) => {
      List.map(o => o##next(el), realObservers^) |> ignore;
    },
    error: (err) => {
      List.map(o => o##error(err), realObservers^) |> ignore;
    },
    complete: () => {
      List.map(o => o##complete(), realObservers^) |> ignore;
    }
  };
  let createResult = createObservable((o) => {
    realObservers := Js_list.cons(o, realObservers^);
  });

  (observer, createResult);
};
```

???

Let's talk about how to bind into methods in an object. Should we have haskell types to compare this to?

Notice that we can't use the subscription to anything, we haven't implemented that feature yet.

Elements are read in order.

What is bs.val about?

Chaining with |> and the type of ignore

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

There is no way of controlling the exports from inside a `.re` file, you need an interface file for that. It can be automatically generated with
```
bsc -bs-re-out lib/bs/src/yourFile.cmi
```

Which will generate an interface file with all the types exported from a module that you can then tweak however you wish.

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

# Parsing JSON (the compositional and parser combinator approaches)


---
class: middle

# Converters and helpers

---
class: middle

# Polymorphic variants

Normal variants must be a member of a type:

```
# type food = Sandwitch | Flakes | Banana;
# Flakes;

- : food = Flakes
```

While polymorphic variants can exist on their own:
```
# `Flakes;

- : [> `Flakes ] = `Flakes
```
and can be used in multiple types:
```
# type foo = [`Flakes | `Sandwitch];
# type bar = [`Flakes];
```

Equality is also the equality of constructors:
```
# type foo = [`One | `Two];
# type bar = [`Two | `One];
# let id = (arg: foo) => arg;
# let test: bar = `One;
# id(test);
- : foo = `One;
```
---


# What is ReasonML:
- compiles to OCaml that uses the bucklescript compiler to output JS
- JS like syntax with more functional features and a typesystem more advanced than Flow/TS but less advanced than Haskell - code is often faster than comparable JS (here some examples)
    -> different function call syntax bracket-languages-like
    -> named components
    -> currying

- easy interop with raw JS 
      -> escape hatch of @bs.raw
      -> call into an arbitrary method, preseve this
      -> multiple ways of doing the same thing
- focus on small, performant and error protected output, at the cost of more advanced features of the typesystem - maybe compare the output of PureScript and Reason for the same sample program, benchmark the output
- show records vs objects, switch statements, importing raw JS
- a bit weird module system, to someone coming from JS/Cpp/Haskell
- describe React a bit, why it is a good paradigm for writing simple apps
- how ReasonReact works?
   -> Go through the tutorial
   -> how events work?

- sooo, what project do we want to have?


---

class: middle

# Can we do better?

---
