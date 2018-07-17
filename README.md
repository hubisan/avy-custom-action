# avy-custom-action

Use `avy-custom-action` to create a custom avy action in Emacs. The
macro creates an interac

Example:

```elisp
;; TODO insert good example here
```

## Usage

```elisp
(avy-custom-action action-name
  [:stay t | nil]
  [:pre (...) | ((...) (...) ...)]
  :actions
  (([:all-windows options]
    [:pre (...) | ((...) (...) ...)]
    :action ((avy-goto function | regexp) [(...) ...])
    [:repeat number]
    [:post (...) | ((...) (...) ...)])
  [:post (...) | ((...) (...) ...))])
```

### Keywords

#### `:stay`

*[optional]* Set to non false value to restore point after the action.

#### `:pre`

*[optional]* Sexp or list of sexps to run before the actions.

#### `:actions`

A list of avy actions with following keywords for each (run in the order shown). For each action executed the point is stored in variable pts (lexical bound) after :post. The variable can be accessed inside the actions or in :post.

- `:all-windows` *[optional]* Change `avy-all-windows` temporarily for the action.
- `:pre` *[optional]* Sexp or list of sexp run before action.
- `:action` Name of a avy-goto function or a regex to use with `avy--generic-jump`.
- `:repeat` *[optional]* Number of times the action should be repeated.
- `:post` *[optional]* Sexp or list of sepx run after action.

#### `:post`

*[optional]* Sexp or list of sexps to run after the actions. All points from the
actions are stored in variable pts. You can use this variable in your
sexp e.g. (car pts)


## Examples

TODO

