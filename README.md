# Tiny-Path

Provides a very light abstraction for windows & unix paths.

## Terminology

- `Path`s are made of `Nodes`
- There are two primary kinds of `Node`, `File` and `Dir`
  - A `Dir` is a non terminal node. It can have nodes after it in a `Path`
  - A `File` is a terminal node. It can not have nodes after it in a `Path`
- A `Root-Dir` is a special `Dir` that can only exist as the first `Node` in a `Path`
- A `Path` is said to be `Absolute` if it has a `Root-Dir`
- A `Path` with no `Root-Dir` is said to be `Relative`

## API

## Why can't `#'make-tpath` make absolute paths

## Escaping

tiny-path does not escape paths, the rules for this are os & shell specific and we arent touching that with a 10 foot pole.
