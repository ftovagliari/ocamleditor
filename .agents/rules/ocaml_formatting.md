---
trigger: always_on
---
# OCaml Formatting Guidelines

- When writing multi-line anonymous functions in OCaml, enclose the function block with `begin` ... `end` instead of parentheses `(...)`.
- Single-line anonymous functions can use parentheses `(...)`.

### Examples

```ocaml
(* Multi-line anonymous function: use begin ... end *)
List.iter begin fun x ->
  let y = x + 1 in
  print_int y
end items;

(* Single-line anonymous function: parentheses are fine *)
List.map (fun x -> x + 1) items
```
