# Lux Token Specification

> ⚠️ **Status:** Lux is under heavy development. Some tokens and features are recognised by the _parser_ but are **not yet implemented** in the compiler or VM. Those are marked **(reserved)** or described in **Planned** sections.

---

## 🔤 Literals

- `Identifier(String)` — user‑defined names (variables, functions, types…)
- `Number(f64)` — numeric literals (`42`, `3.14`)
- `String(String)` — string literals (`"hello"`)
- `True` / `False` — boolean literals

---

## 🗝️ Keywords

| Keyword                     | Purpose                                   | Implementation Status                    |
| --------------------------- | ----------------------------------------- | ---------------------------------------- |
| `Let`                       | variable binding (⛔ not at global scope) | ✅                                       |
| `Const`                     | constant binding (allowed globally)       | ✅                                       |
| `Fn`                        | function definition                       | ✅                                       |
| `Return`                    | return from a function                    | ✅                                       |
| `If`, `Else`                | conditional branching                     | ✅                                       |
| `For`                       | loop construct                            | ✅                                       |
| `Switch`, `Case`, `Default` | multi‑branching                           | ✅                                       |
| `Break`, `Continue`         | loop control                              | ✅                                       |
| `Refraction`                | type alias / named type                   | ✅                                       |
| `Facet`                     | structured data type                      | ✅                                       |
| `Interface`                 | contract type                             | **(reserved)**                           |
| `Constellation`             | declares a module                         | **Parser‑only** — semantic linking _TBD_ |
| `Import`, `As`              | bring external module, rename             | **(reserved)** — not parsed yet          |

---

## 🧬 Native Types

| Lux Type           | Conventional Equivalent | Purpose                                                                |
| ------------------ | ----------------------- | ---------------------------------------------------------------------- |
| `Umbra`            | `void`/`nil`            | absence of value                                                       |
| `Light`            | `int`/`float`           | numeric values                                                         |
| `Lumens`           | `string`                | textual data                                                           |
| `Photon`           | `bool`                  | truth values                                                           |
| `[T]`              | `Array<T>`              | homogeneous collections (indexing supported; `push`/`pop` **planned**) |
| `Function([T], T)` | `fn(T) -> T`            | function type                                                          |

---

## ⚙️ Native Functions

Currently only one builtin:

- `emit(value)` — print a value (Lux’s `print`)

Additional std‑lib style functions will arrive with the module system.

---

## ➕ Operators

### Arithmetic

`+  –  *  /  %`

### Comparison

`==  !=  <  <=  >  >=`

### Logical

`!  &&  ||`

### Assignment

`=`

---

## ⚖️ Operator Precedence & Associativity

| Precedence | Operators            | Associativity |
| ---------- | -------------------- | ------------- |
| 7          | `!`, unary `-`       | Right‑to‑left |
| 6          | `*`, `/`, `%`        | Left‑to‑right |
| 5          | `+`, `-`             | Left‑to‑right |
| 4          | `<`, `<=`, `>`, `>=` | Left‑to‑right |
| 3          | `==`, `!=`           | Left‑to‑right |
| 2          | `&&`                 | Left‑to‑right |
| 1          | `\|\|`               | Left‑to‑right |
| 0          | `=`                  | Right‑to‑left |

---

## 🌌 Module System — **Planned Design**

`Constellation` and `import` outline Lux’s future modular architecture, but linker‑level semantics are **not yet live**. The current compiler treats each `.lux` file as a standalone program.

### Planned Rules

1. **Constellation blocks a folder**: every `.lux` file in the same directory shares the same namespace.
2. Global `const` values are visible across files in the same constellation — no `import` needed.
3. To reference another folder’s constellation, use `import "name"` (syntax reserved).

```text
src/
├── main.lux        # import "std"   (planned)
└── std/
    ├── math.lux    # constellation std;
    └── geometry.lux
```

Once implemented your code will look like:

```lux
// std/math.lux
constellation std;

const PI: Light = 3.1415;

// main.lux
import "std";

emit std.PI;
```

---

_Last updated: Apr 18 2025_
