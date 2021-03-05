# λ evaluator

## Currently working on implementations of Untyped, Simply Typed, and System F (λ, λ->, λ2)

### To test yourself:

```
$ stack build
```

#### Untyped
```
$ stack run untyped
```

#### Simply Typed
```
$ stack run simply
```

#### System F
```
$ stack run systemF
```

## Controls:
- write `λ` as `\`
- write `Λ` as `/`
- use numbers such as `23`
- Simple Types: `Nat` and `Bool`
- System F Types: `Nat` and `Bool`
- write System F type arguments as `[Nat]`
- you can omit redundant parentheses both in expressions and in type annotations

## Commands:
- `:step` for the next step of the evaluation
- `:type` for type annotation of the current expression
- `:isnormal` for checking if the expression is in the normal form
- `:applyto` for adding an argument to the current expression
- `:normalize` for the evaluation until the normal form is reached
- `:reset` for inputting different expression
- `:bye` for exiting the program

## Examples:

```
[enter λ expression]
:$ a b c (d e f g) h i
```


### Examples of the λ->

Following example starts with submitting the `(\ f : Bool -> Bool -> Bool . (\ b : Bool . f b b))` expression.
It is then applied to `(\ t : Bool . (\ f : Bool . t))`.
The resulting `application` is then applied to the `True`.

Each change results in printing the current expression together with it's type.

At the end we just normalize the whole term and check the result.

```
  [enter λ-> expression]
  λ-> >> (\ f : Bool -> Bool -> Bool . (\ b : Bool . f b b))
        (λ f : Bool -> Bool -> Bool . (λ b : Bool . f b b)) :: (Bool -> Bool -> Bool) -> Bool -> Bool
  
  [enter command or λ-> expression]
  λ-> >> :applyto
  λ-> >> (\ t : Bool . (\ f : Bool . t))
        (λ f : Bool -> Bool -> Bool . (λ b : Bool . f b b)) (λ t : Bool . (λ f : Bool . t)) :: Bool -> Bool
  
  [enter command or λ-> expression]
  λ-> >> :applyto
  λ-> >> True
        (λ f : Bool -> Bool -> Bool . (λ b : Bool . f b b)) (λ t : Bool . (λ f : Bool . t)) True :: Bool
  
  [enter command or λ-> expression]
  λ-> >> :normalize
        True :: Bool
```


On the other hand following example just shows step by step evaluation of the well typed term.

```
  [enter λ-> expression]
  λ-> >> (\ a : Bool -> Nat . (\ b : Bool . a b)) (\ e : Bool . 23) True
        (λ a : Bool -> Nat . (λ b : Bool . a b)) (λ e : Bool . 23) True :: Nat
  [enter command or λ-> expression]
  λ-> >> :step
  (λ b : Bool . (λ e : Bool . 23) b) True
        (λ b : Bool . (λ e : Bool . 23) b) True :: Nat
  [enter command or λ-> expression]
  λ-> >> :step
  (λ e : Bool . 23) True
        (λ e : Bool . 23) True :: Nat
  [enter command or λ-> expression]
  λ-> >> :step
  23
        23 :: Nat
```

### Examples of the System F

```
  [enter λ2 expression]
  λ2 >> (\ a : forall A . A -> A -> A . a) (/ B . (\t : B . (\f : B . t)))
        (λ a : (forall A . A -> A -> A) . a) (Λ B . (λ t : B . (λ f : B . t))) :: (forall A . A -> A -> A)
```

```
  [enter λ2 expression]
  λ2 >> (\ a : forall A . A -> A -> A . a) (/ B . (\t : B . (\f : B . t))) [Nat]
        (λ a : (forall A . A -> A -> A) . a) (Λ B . (λ t : B . (λ f : B . t))) [Nat] :: Nat -> Nat -> Nat
  
  [enter command or λ-> expression]
  λ2 >> :step
  (Λ B . (λ t : B . (λ f : B . t))) [Nat]
        (Λ B . (λ t : B . (λ f : B . t))) [Nat] :: Nat -> Nat -> Nat
  
  [enter command or λ-> expression]
  λ2 >> :step
  (λ t : Nat . (λ f : Nat . t))
        (λ t : Nat . (λ f : Nat . t)) :: Nat -> Nat -> Nat
  
  [enter command or λ-> expression]
  λ2 >> :applyto
  λ2 >> 23
        (λ t : Nat . (λ f : Nat . t)) 23 :: Nat -> Nat
  
  [enter command or λ-> expression]
  λ2 >> :step
  (λ f : Nat . 23)
        (λ f : Nat . 23) :: Nat -> Nat
  
  [enter command or λ-> expression]
  λ2 >> :applyto
  λ2 >> 42
        (λ f : Nat . 23) 42 :: Nat
  
  [enter command or λ-> expression]
  λ2 >> :step
  23
        23 :: Nat
```

<!--

```
[enter λ expression]
:$ (λ f . (λ x . (f (x x))) (λ x . (f (x x)))) (λ f . (λ n . ((λ n . n (λ x . (λ t . (λ f . f))) (λ t . (λ f . t))) n (λ s . (λ z . s z)) ((λ x . (λ y . (λ s . (x (y s))))) n (f ((λ m . (λ n . n (λ x . (λ s . (λ z . x (λ f . (λ g . (g (f s)))) (λ g . z) (λ u . u)))) m)) n (λ s . (λ z . s z)))))))) (λ s . (λ z . (s (s (s (s (s (s z))))))))
[command or expression]:$ :normalize
:$ (λ s . (λ z . (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
```
-->

More coming (hopefully) soon.