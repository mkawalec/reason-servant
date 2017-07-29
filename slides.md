class: center, middle

# Functional Programming with Bananas in Barbed Wire

[@monad_cat](https://twitter.com/monad_cat)

???

The title is from a paper from 91! The paper is pretty dry, but it's
worth reading AFTER you know what this thing is. You'd die otherwise.

---

class: middle

The usual way for writing recursion goes like
```
data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr
```

And we can pretty-print the AST like
```
print :: Expr -> String
print (Const i) = show i
print (Add a b) = print a ++ " + " ++ print b
print (Mul a b) = print a ++ " * " ++ print b
```

---

class: middle

# Can we do better?

```
data ExprF a = Const Int
             | Add a a 
             | Mul a a
             deriving (Show, Eq)
```

which corresponds to

```
data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr
```
---
class: middle

# Is this a functor?

```
data ExprF a = Const Int
             | Add a a 
             | Mul a a
             deriving (Show, Eq)

class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Let's try writing an fmap

```
fmap f (Const i) = Const i
fmap f (Add a a) = Add (f a) (f a)
fmap f (Mul a a) = Mul (f a) (f a)
```

`Const i` case is forced by types of fmap (`i` is always `Int`, cannot
be `b`)

---
class: middle
```
data ExprF a = Const Int
             | Add a a 
             | Mul a a
             deriving (Show, Eq)

fmap f (Const i) = Const i
fmap f (Add a a) = Add (f a) (f a)
fmap f (Mul a a) = Mul (f a) (f a)
```

Functor laws are
```
fmap id == id
fmap (f . g) == fmap f . fmap g
```

Proof of (1) follows from definition, for (2)
```
Add (f . g $ a) (f . g $ b) = fmap f (Add (g a) (g b)) = 
  fmap f . fmap g $ Add a b
```

We can get all this done for us with `DeriveFunctor`, but that's not the
point

---

class: middle

# Our recursive type

```
data ExprF a = Const Int
             | Add a a 
             | Mul a a
```

But this is useless, right?

```
> :t Const 2
> Const 2 :: ExprF a

> :t Add (Const 1) (Const 2)
> Add (Const 1) (Const 2) :: ExprF (ExprF a)
```

---

class: middle

# Fixed point

```
data Fix f = Fix (f (Fix f))  --or
data Fix f = Fix {outF :: f (Fix f)}
```

Fixed point is a value that is mapped to itself by a function

```
Fix (f (Fix f)) = Fix (f (Fix (f (Fix f))))
```

How do we use it?
```
Mul (Add ((Const 2) (Const 2))) (Const 2)

Fix $ Const 2

> :t Fix $ Const 2
Fix $ Const 2 :: Fix ExprF

let fixedExpr =
  Fix (Mul (Fix (Add (Fix $ Const 2) (Fix $ Const 2))) (Fix $ Const 2))

> :t fixedExpr
... :: Fix ExprF
```
---

class: middle

# Algebras / Catamorphisms

```
type Algebra f a = f a -> a

data ExprF a = Const Int
             | Add a a 
             | Mul a a

printAlg :: ExprF String -> String
printAlg (Const i) = show i
printAlg (Add a b) = "(" ++ a ++ " + " ++ b ++ ")"
printAlg (Mul a b) = a ++ " * " ++ b

data Fix f = Fix {outF :: f (Fix f)}
let fixedExpr =
  Fix (Mul (Fix (Add (Fix $ Const 2) (Fix $ Const 2))) (Fix $ Const 2))
```

We can write a pretty-printing algebra for `ExprF`, but is it of any
use?

```
cata :: Functor f => (f b -> b) -> Fix f -> b
cata f = f . fmap (cata f) . outF

> cata printAlg fixedExpr
"(2 + 2) * 2"
```

---

class: middle

```
let fixedExpr =
  Fix (Mul (Fix (Add (Fix $ Const 2) (Fix $ Const 2))) (Fix $ Const 2))

cata :: Functor f => (f b -> b) -> Fix f -> b
cata f = f . fmap (cata f) . outF

getValue :: ExprF Int -> Int
getValue (Const i) = i
getValue (Add a b) = a + b
getValue (Mul a b) = a * b

> cata getValue fixedExp
8
```
---

class: middle

# Anamorphisms

For unfolds we need an opposite of an algebra

```
type Coalgebra f a = a -> f a

unwrap :: Coalgebra ExprF Int
unwrap i
  | i < 4     = Add (i + 1) (i + 2)
  | otherwise = Const i
```

Can we just be lazy and flip around the types in `cata`?

```
cata f = f . fmap (cata f) . outF
ana f = Fix . fmap (ana f) . f

> :t ana
ana :: Functor f => (a -> f a) -> a -> Fix f
```

```
ana unwrap 1 --No instance for (Show (Fix ExprF)) arising from a use of ‘print’

> cata printAlg $ ana unwrap 1
"(((4 + 5) + 4) + (4 + 5))"
```
---

class: middle

# Paramorphisms

Stages that aren't leaves have no access to the original elements.
Parsing folded is silly and often impossible.

```
type RAlgebra f a = f (Fix f, a) -> a
```

We get a tuple of an original element and the folded value

```
para :: forall f a . (Functor f) => RAlgebra f a -> Fix f -> a
para rAlg = rAlg . fmap fanout . outF
  where fanout :: Fix f -> (Fix f, a)
        fanout t = (t, para rAlg t)
```

We can sum the additions up for a shorter output

```
concatSums :: RAlgebra ExprF String
concatSums (Const i) = show i
concatSums (Add (aExpr, _) (bExpr, _)) = 
  show $ cata getValue aExpr + cata getValue bExpr
concatSums (Mul (_, a) (_, b)) = a ++ " * " ++ b
```

```
> para concatSums fixedExpr
4 * 2
```

---

class: middle

# Practical example, rotating a square

```
.x.. | .x..
..x. | .x.x
xxx. | .xx.
.... | ....

data QuadTreeF a r =
    NodeF r r r r
  | LeafF a
  | EmptyF
type QuadTree a = Fix (QuadTreeF a)
```

We need some helper functions

```
node :: QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a
node ul ur lr ll = Fix (NodeF ul ur lr ll)

leaf :: a -> QuadTree a
leaf = Fix . LeafF

empty :: QuadTree a
empty = Fix EmptyF
```
---

class: middle

```
.x..
..x.
xxx.
....

tree :: QuadTree Bool
tree = node ul ur lr ll where
  ul = node (leaf False) (leaf True) (leaf False) (leaf False)
  ur = node (leaf False) (leaf False) (leaf False) (leaf True)
  lr = node (leaf True) (leaf False) (leaf False) (leaf False)
  ll = node (leaf True) (leaf True) (leaf False) (leaf False)

rotate :: QuadTree a -> QuadTree a
rotate = cata $ \case
  NodeF ul ur lr ll -> node ll ul ur lr
  LeafF a           -> leaf a
  EmptyF            -> empty

> rotate tree
.x..
.x.x
.xx.
....

```

It rotates!
---

class: middle

# Why recursion schemes?

- clearer what is happening to individual elements in recursion -> less bugs
- recursive data types make reasoning about certain problems easier
- (sometimes) better compiler optimization of complex nonrecursive functions. Benchmarks from [vmchale/morphism-zoo](https://github.com/vmchale/morphism-zoo) execute with basically the same times for all methods unless the case is pretty degenerate
- common vocabulary to reason about walking the trees
- can you spot a similarity to the Free Monad?

```
    data Free f r = Free (f (Free f r)) | Pure r
```


[@monad_cat](https://twitter.com/monad_cat)
