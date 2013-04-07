# CHANGELOG

## 0.1.0

* The first argument to `Handler:handle/3` now is a binary string instead of an
  atom.
* The `response` record has been removed. Instead, a tuple with the status code
  and body, or status code, headers and body can be returned from
  `Handler:handle/3`.
