# avy-custom-action

Use `avy-custom-action` to create a custom avy action in Emacs.

Example:

```elisp
;; TODO insert good example here
```

## Syntax

```elisp
(avy-custom-action name args)
```

Creates an interactive function named `name` for the custom avy action defined with `args`.

### Parameters

- `name`: Name to use for the interactive function that will be created, e.g. `my-avy-action`.
- `args`: Plist with predefined keywords as properties, see [Usage](#Usage) and [Keywords](#Keywords).

## Usage
```elisp
(avy-custom-action action-name
  [:stay t | nil]
  [:pre (sexp) | ((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])]
  :actions
  (([:all-windows t | nil | 'all-frames]
    [:pre (...) | ((...) (...) ...)]
    :action avy-goto-function-name | regexp
    [:repeat number]
    [:post (sexp) | ((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])])
   [(action2) [... [(actionN)]]])
  [:post (sexp) | ((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])])
```

### Keywords

#### `:stay`

*[optional]* `t` or `nil`
Set to non false value to restore point after the action.

#### `:pre`

*[optional]* `(sexp)` or `((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])`
Sexp or list of sexps to run before the actions.

#### `:actions`
```
(([:all-windows t | nil | 'all-frames]
   [:pre (...) | ((...) (...) ...)]
   :action avy-goto-function-name | regexp | any-function
   [:repeat number]
   [:post (sexp) | ((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])])
  [(action2) [... [(actionN)]]])   [(action2) [... [(actionN)]]])
```

A list of avy actions with following keywords for each (run in the order shown)

- **`:all-windows`** *[optional]* `t` or| `nil` or `'all-frames`  
Change `avy-all-windows` temporarily for the action.
- **`:pre`** *[optional]* `(sexp)` or `((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])`  
Sexp or list of sexp run before action.
- **`:action`** `avy-goto-function-name` or `regexp`  
Name of an avy-goto function or a regex to use with `avy--generic-jump`.
- **`:repeat`** *[optional]* `number`  
Number of times the action should be repeated.
- **`:post`** *[optional]* `(sexp)` or `((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])`  
Sexp or list of sexps run after action.

For each action executed the point is stored in variable `pts` (lexical bound) after `:action` > `:post`. The variable can be accessed inside the actions (`:pre`, `:post`) or in `:post`.

#### `:post`

*[optional]* `(sexp)` or `((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])`

Sexp or list of sexps to run after the actions. All points from the
actions are stored in variable `pts`. You can use this variable in your
sexp e.g. `(car pts)`.

## Examples

TODO

### Contribution

TODO
