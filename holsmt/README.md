# Holrs

A Higher-Order Logic prover written in Rust.

## Getting Started

Before you can use Holrs, you need to generate the `parser.rs` file, which is too large to include in the Git repository. You can do this by following these steps:

1. Clone this repository to your local machine:

```shell
git clone https://gitee.com/jiamiao532/hol-kernel-rust.git
```
2. Navigate to the project directory:
```shell
cd hol-kernel-rust
```
3. Build the project to generate the `parser.rs` file using Cargo:
```shell
cargo build
```
This will invoke the LALRPOP parser generator and create the parser.rs file.