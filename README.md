# Advent of Code 2016 in Haskell

This repository contains my solutions to the [Advent of Code 2016](https://adventofcode.com/2016) challenges, implemented in Haskell. The goal of this project is to improve my Haskell skills while also getting familiar with the [Nix](https://nixos.org/) environment.

## Description

The Advent of Code is an annual set of Christmas-themed programming challenges that are published daily in December. This repository hosts my solutions for the 2016 edition. Each day's challenge is solved in Haskell, using a Nix environment to manage dependencies and build tools. This is a small recreational project, aimed at enhancing my functional programming skills and exploring the Nix ecosystem.

## Installation

To set up the project environment and run the solutions, follow these steps:

1. **Install Nix**: Make sure you have [Nix](https://nixos.org/download.html) installed on your system.

2. **Enter the Nix shell**: Run the following command to enter a shell with the necessary dependencies:

   ```bash
   nix-shell --attr env release.nix

Great! Based on the information you've provided, here's a `README.md` for your Haskell project, **Advent of Code 2016 in Haskell**. Feel free to modify any part of it to better fit your needs:

# Advent of Code 2016 in Haskell

This repository contains my solutions to the [Advent of Code 2016](https://adventofcode.com/2016) challenges, implemented in Haskell. The goal of this project is to improve my Haskell skills while also getting familiar with the [Nix](https://nixos.org/) environment.

## Description

The Advent of Code is an annual set of Christmas-themed programming challenges that are published daily in December. This repository hosts my solutions for the 2016 edition. Each day's challenge is solved in Haskell, using a Nix environment to manage dependencies and build tools. This is a small recreational project, aimed at enhancing my functional programming skills and exploring the Nix ecosystem.
This is work in progress.

## Installation

To set up the project environment and run the solutions, follow these steps:

1. **Install Nix**: Make sure you have [Nix](https://nixos.org/download.html) installed on your system.

2. **Enter the Nix shell**: Run the following command to enter a shell with the necessary dependencies:

   ```bash
   nix-shell --attr env release.nix
   ```

3. **Run the project**: Once in the shell, execute the following command to run the solutions:

   ```bash
   cabal run
   ```

## Usage

The solutions for each day are organized as follows:

- The main entry point for the project is `app/Main.hs`.
- Each day's solution is implemented in a separate file under `src/`, named as `dayXX.hs` (where `XX` represents the day number).

To run a specific day's solution, you can modify the `app/Main.hs` file to call the desired function.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.
