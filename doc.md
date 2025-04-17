# Lux Token Specification

This document outlines the complete set of tokens used in the Lux programming language.

---

## 🔤 Literals

These are direct values or identifiers in the source code.

- `Identifier(String)` — user-defined names for variables, functions, types, etc.
- `Number(f64)` — numeric literals (e.g. `42`, `3.14`)
- `String(String)` — string literals (e.g. `"hello"`)
- `True` — boolean literal `true`
- `False` — boolean literal `false`

---

## 🗝️ Keywords

Reserved words that define the structure of Lux programs.

- `Let` — variable binding (not allowed at global scope)
- `Const` — constant binding (allowed at global scope)
- `Fn` — function definition
- `Return` — function return statement
- `If` — conditional branching
- `Else` — alternate conditional branch
- `For` — loop construct
- `Switch` — pattern matching/multi-branching
- `Case` — branch condition in a `switch`
- `Default` — fallback branch in a `switch`
- `Break` — exit from loops or switches
- `Continue` — skip to next iteration in loops
- `Refraction` — create an alias or new type
- `Facet` — structured data type
- `Interface` — contract type
- `Import` — bring external module
- `As` — rename on import

---

## 🧬 Native Types

Built-in types recognized by the Lux VM.

- `Umbra` — the absence of a value (null/nil)
- `Light` — number type (f64)
- `Lumens` — string type
- `Photon` — boolean type
- `[T]` — array of values of type `T`
- `Function([T], T)` — function taking arguments of type `[T]` and returning type `T`
- `(T) -> U` — alternate syntax for function types

---

## ⚙️ Native Functions

Built-in functions available globally.

- `Emit` — output/print a value (Lux's version of `print`)

---

## ➕ Operators

Tokens representing arithmetic, comparison, and logical operations.

### Arithmetic:

- `+` — addition
- `-` — subtraction or negation
- `*` — multiplication
- `/` — division
- `%` — modulo

### Comparison:

- `==` — equality
- `!=` — inequality
- `<` — less than
- `<=` — less than or equal
- `>` — greater than
- `>=` — greater than or equal

### Logical:

- `!` — logical not
- `&&` — logical and
- `||` — logical or

### Assignment:

- `=` — assignment

---

## ⚖️ Operator Precedence & Associativity

Defines how operators group when parentheses are not used (from highest to lowest).

| Precedence | Operators            | Description               | Associativity |
| ---------- | -------------------- | ------------------------- | ------------- |
| 7          | `!`, `-`             | Logical NOT, unary prefix | Right-to-left |
| 6          | `*`, `/`, `%`        | Multiplicative            | Left-to-right |
| 5          | `+`, `-`             | Additive                  | Left-to-right |
| 4          | `<`, `<=`, `>`, `>=` | Comparison                | Left-to-right |
| 3          | `==`, `!=`           | Equality                  | Left-to-right |
| 2          | `&&`                 | Logical AND               | Left-to-right |
| 1          | `\|\|`               | Logical OR                | Left-to-right |
| 0          | `=`                  | Assignment                | Right-to-left |

---

## 💡 Notes

- Tokens are defined in the lexer and used by the parser for syntactic structure.
- Native types and functions are treated specially during compilation.
- Operator precedence and associativity guide parsing of expressions.
- This list may grow as the standard library evolves.

---

## 🌌 Module System (Constellations)

In Lux, source files are grouped into modules using the `Constellation` keyword.

### 💫 Visibility Rules

- All files inside the same folder are part of the same `Constellation`.
- Global `const` bindings are **shared across files** in the same `Constellation` — no `import` needed.
- To access definitions from a different folder, you must explicitly `import` that `Constellation`.

### 📁 Example

```
src/
├── main.lux → import "std"
└── std/
    ├── math.lux → Constellation std
    └── geometry.lux → Constellation std
```

```lux
// std/math.lux
Constellation std;

const PI: Light = 3.1415;

// std/geometry.lux
// No import needed, same constellation

const TWOPI = PI * 2;

// main.lux
import "std";

emit std.TWOPI; // OK
```

---

Last updated: Apr 15, 2025
