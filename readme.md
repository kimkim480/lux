<p align="center">
  <img src="assets/lux_header.png" alt="Lux Banner">
</p>

---

## What is Lux?

- **Lux** is a statically typed, stack-based programming language designed for learning and exploration.
- It is built from scratch in **Rust** with a custom virtual machine called **Prism**.
- Its purpose: to shine light through the chaos of runtime errors — with clarity, intention, and precision.

## Goals of Lux

- **Clarity** over cleverness
- **Static typing** from the ground up
- **Minimalist syntax**, inspired by light and structure
- Built to **learn**, not necessarily to ship

---

## ✨ Features

- 🧠 Statically typed with `Light`, `Lumens`, `Photon`, `Umbra`
- 🔁 Control flow: `for`, `if`/`else`, `switch`, `break`, `continue`
- 🧮 Arithmetic, logic, and comparison operators
- 🧱 Facets (structs) and type aliases (`Refraction`)
- 🧵 Function declarations, closures, and return values
- 📦 Arrays with method-style operations (`push`, `pop`, indexing)
- 🔦 Function types (`Function([T], T)` or `(T) -> T`) for higher-order programming
- 🌌 Modular system using `Constellation` and `import`

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

## 🧪 Examples

```lux
// Numeric and string
let x: Light = 42;
let name: Lumens = "Lux";

// Boolean logic
let isBright: Photon = true && false;

// Array
let numbers: [Light] = [1, 2, 3, 4];
emit numbers[2]; // emits 3

// Function type (Lambda)
let greet: () -> Umbra = fn() {
  emit "Hello from Lux!";
};
greet();

// Function definition
fn add(a: Light, b: Light) -> Light {
  return a + b;
}
let sum: Light = add(1, 2);

// Facet (struct)
Refraction Point Facet {
  x Light
  y Light
};

let p: Point = Point { x: 3, y: 4 };
emit p.x + p.y; // emits 7

// Type alias
Refraction Lambda Function([Light, Light], Light)

let add: Lambda = fn(a: Light, b: Light) -> Light {
  return a + b;
};

let sum: Light = add(1, 2);
```

---

## 🛠 Installation

You’ll need [Rust](https://www.rust-lang.org/tools/install) installed. Then:

```bash
cargo install lux
```

This will install two CLI tools:

- `lux` – the compiler (produces bytecode)
- `prism` – the virtual machine (runs bytecode)

---

## 🚀 Getting Started

Create a file called `hello.lux`:

```lux
constellation main;

fn Prism() {
  emit "Hello, world!";
}
```

Then compile and run it:

```bash
lux hello.lux
```

Or compile and run it with Prism manually:

```bash
lux hello.lux -o hello.prism
prism hello.prism
```

---

Made with ☀️ — _Post Tenebras Lux_
