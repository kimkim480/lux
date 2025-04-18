<p align="center">
  <img src="assets/lux_header.png" alt="Lux Banner">
</p>

---

## What is Lux?

- **Lux** is a statically typed, stack‑based programming language designed for learning and exploration.
- It is built from scratch in **Rust** with a custom virtual machine called **Prism** (⚠️ _under active development_).
- Its purpose: to shine light through the chaos of runtime errors — with clarity, intention, and precision.

---

## ✨ Current Features

| Status | Feature                                                          | Notes                                                                                  |
| ------ | ---------------------------------------------------------------- | -------------------------------------------------------------------------------------- |
| ✅     | **Static type system** with `Light`, `Lumens`, `Photon`, `Umbra` |                                                                                        |
| ✅     | Control flow: `for`, `if / else`, `switch`, `break`, `continue`  |                                                                                        |
| ✅     | Arithmetic, logic and comparison operators                       |                                                                                        |
| ✅     | **Facets** (structs) and type aliases (**Refraction**)           |                                                                                        |
| ✅     | Function declarations, closures, and explicit return types       |                                                                                        |
| ✅     | Arrays with **indexing** (`xs[0]`)                               | `push`/`pop` methods are on the roadmap                                                |
| ✅     | Higher‑order functions via `Function([T], T)` syntax             |                                                                                        |
| 🛠️     | Modular system using `Constellation` and `import`                | Parser recognizes `constellation`, but module/import semantics are not implemented yet |
| 🛠️     | Stand‑alone **Prism** CLI to run compiled byte‑code              | For now, `lux` compiles _and_ runs code                                                |

> _Planned features are marked with the wrench (🛠️) icon._

---

## 🧪 Primitive Types in Lux

| Lux Type           | Conventional Equivalent | Purpose                                   | "Why This Name?"                                       |
| ------------------ | ----------------------- | ----------------------------------------- | ------------------------------------------------------ |
| `Light`            | `int`/`float`           | Numeric values (all sizes)                | Fundamental energy — numbers as the measurable force   |
| `Lumens`           | `string`                | Textual data                              | Measured brightness — strings as visible communication |
| `Umbra`            | `void`/`nil`/`null`     | Absence of value                          | The deepest shadow — no light, no value                |
| `Photon`           | `bool`                  | Truth values (true/false)                 | Quantum particle of light — binary truth               |
| `[T]`              | `Array<T>`              | Homogeneous collections                   | A spectrum of values bound by the same light           |
| `Function([T], T)` | `fn(T) -> T`            | Function that takes `T` and returning `T` | A beam directed — computation shaped with intention    |

---

## 🚀 Quick Tour

```lux
// Declare values
let x: Light = 42;
let name: Lumens = "Lux";

// Boolean logic
let ok: Photon = true && !false;

// Arrays
let nums: [Light] = [1, 2, 3];
emit nums[1]; // → 2

// Functions
fn add(a: Light, b: Light) -> Light {
  return a + b;
}
let sum: Light = add(3, 4);

// Facets (struct‑like)
Refraction Point Facet {
  x Light
  y Light
}
let p: Point = Point { x: 3, y: 4 };
emit p.x + p.y; // → 7
```

---

## 🛠 Installation

> You need [Rust](https://www.rust-lang.org/tools/install) installed.

```bash
cargo install lux-lang
```

This command installs **one** executable for now:

- `lux` – _compiler **and** interpreter_ (runs code directly)

A separate `prism` CLI that runs byte‑code is planned, but not yet available.

---

## 🏃‍♂️ Running Your First Program

Create a file named **hello.lux**:

```lux
constellation main;

fn Prism() {
  emit "Hello, world!";
}
```

Then run:

```bash
lux hello.lux
```

That’s it! The `lux` tool will compile your code, execute it, and print the output.

> Byte‑code output (`-o <file>.prism`) and the stand‑alone `prism` runner will be documented once available.

---

Made with ☀️ — _Post Tenebras Lux_
