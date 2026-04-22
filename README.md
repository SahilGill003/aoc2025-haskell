# Advent of Code 2025 — Haskell

Solutions for [Advent of Code 2025](https://adventofcode.com/2025) written in Haskell.

## Requirements

- [GHC](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/)

## Usage

### 1. Add your input

Paste the puzzle input for the day into the test case file before running:

```
testcase.txt
```

### 2. Build

```bash
cabal build
```

### 3. Run a specific day

```bash
cabal exec aoc2025 day1
cabal exec aoc2025 day2
# etc.
```

## Solutions

| Day  | Status |
|----- |--------|
| Day 1  | ✅ Solved |
| Day 2  | ✅ Solved |
| Day 3  | ✅ Solved |
| Day 4  | ✅ Solved |
| Day 5  | ✅ Solved |
| Day 6  | ✅ Solved |
| Day 7  | ✅ Solved |
| Day 8  | ✅ Solved |
| Day 9  | ✅ Solved |
| Day 10 | ❌ Solved |
| Day 11 | ✅ Not solved |
| Day 12 | ❌ Not solved |

> 10 / 12 solved

## Project Structure

```
aoc2025/
├── app/
│   └── Main.hs         -- Entry point, dispatches by day argument
│   ├── Day1.hs
│   ├── Day2.hs
│   └── ...
├── testcase.txt        -- Paste puzzle input here before running
└── aoc2025.cabal
```
