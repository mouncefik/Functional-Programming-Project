![image](https://github.com/user-attachments/assets/c944da1e-c91d-4f67-8d33-d681c55384b6)# Functional Programming Algorithms

This repository contains implementations of classic algorithms and problems using the functional programming paradigm, primarily in OCaml. The project is structured into three main parts, each tackling a different challenge: implementing a Tic-Tac-Toe game with AI, solving the Knapsack Problem using Dynamic Programming, and optimizing the Traveling Salesperson Problem (TSP).

The goal of this project is to demonstrate the application of functional programming principles such as immutability, pure functions, recursion, and higher-order functions to solve algorithmic tasks effectively.

## Project Structure

The repository is organized into directories for each main project part:

*   `Knapsack/`: Contains the implementation for the Knapsack Problem.
    *   `knapsack.ml`
*   `Tic-Tac-Toe/`: Contains the implementation for the Tic-Tac-Toe game with AI.
    *   `Alpha_Beta.ml`: Logic for the Alpha-Beta pruning optimization.
    *   `CompleteGame.ml`: The main entry point for the game logic and user interface.
    *   `GameModeling.ml`: Functions for representing and manipulating the game state (board).
    *   `MinMax.ml`: Implementation of the core Minimax algorithm.
*   `Traveling-Salesperson-Problem/`: Contains implementations for different approaches to the Traveling Salesperson Problem.
    *   `BruteForce.ml`: Implementation of the brute force approach.
    *   `GeneticAlgorithm.ml`: Implementation of a Genetic Algorithm approach.
    *   `GreedyAlgorithm.ml`: Implementation of the greedy algorithm.
    *   `bruteforce_greedy.ml`: file combining and comparing brute force and greedy.
  

## Technologies Used

*   **OCaml**: The primary programming language used for all implementations.

## How to Run

This project requires the OCaml compiler installed on your system.

1.  **Install OCaml:** If you don't have OCaml, install it (using `opam` is highly recommended: `opam init` then `opam install ocaml`).
2.  **Clone the repository:**
    ```bash
    git clone https://github.com/mouncefik/Functional-Programming-Project
    cd Functional-Programming-Project
    ```
3.  **Compile and Execute:** Navigate into the specific directories (`Knapsack`, `Tic-Tac-Toe`, or `Traveling-Salesperson-Problem`). You will need to compile the `.ml` files for each part and run the main executable or script.

    *Example (Illustrative - exact command depends on main entry point and potentially build system):*
    ```bash
    # For Tic-Tac-Toe:
    cd Tic-Tac-Toe/
    # Compile the source files (order might matter for dependencies)
    ocamlc -o tictactoe CompleteGame.ml
    # Run the game
    ./tictactoe

    # For Knapsack:
    cd Knapsack/
    ocamlc -o knapsack knapsack.ml
    ./knapsack 

    # For TSP:
    cd Traveling-Salesperson-Problem/
    # Compile and run the desired algorithm (e.g., Brute Force)
    ocamlc -o tsp_bruteforce BruteForce.ml
    ./tsp_bruteforce
    ```
    Refer to the source code comments within each file for more specific execution instructions for each part if necessary.

## Project Parts & Features

### Part 1: Tic-Tac-Toe with Minimax & Alpha-Beta

Implementation of the classic Tic-Tac-Toe game featuring an AI opponent.

*   Functional representation of the game board and state.
*   Implementation of the recursive **Minimax** algorithm to determine the optimal move for the AI.
*   Optimization of the Minimax search using **Alpha-Beta Pruning** to significantly reduce the number of states explored.
*   Simple command-line interface for player interaction.

### Part 2: Knapsack Problem (Dynamic Programming)

Solving the 0/1 Knapsack problem using Dynamic Programming techniques within the functional paradigm.

*   Defining the problem structure and recursive relation for DP.
*   Implementing the DP solution, focusing on avoiding recomputing solutions to overlapping subproblems.
*   Exploring either a top-down approach with memoization or a bottom-up approach.

### Part 3: Traveling Salesperson Problem (TSP)

Finding optimal or near-optimal routes for the Traveling Salesperson Problem using functional approaches.

*   Representing cities and distances using immutable data structures.
*   Functions for generating permutations of cities (routes) and calculating their total cost (distance).
*   Implementations of optimization algorithms:
    *   **Brute Force:** Exploring all possible permutations to find the absolute shortest path (feasible only for small numbers of cities).
    *   **Greedy Algorithm:** A heuristic approach that builds a route by always selecting the nearest unvisited city.
    *   **Genetic Algorithm:** *(Implementation included)* A bio-inspired optimization algorithm suitable for larger instances of the TSP.

## Demo

A demonstration video showcasing the **Tic-Tac-Toe game** with its AI functionality is available here:

---

![demo](https://github.com/user-attachments/assets/6be7e7bc-5a32-403e-b422-11b07bea9093)


## Report

This code repository complements a detailed project report which provides theoretical background, design choices, implementation details, test results, performance analysis, and reflections on the use of functional programming for these problems.

## Author

*   Mouncef IKhoubi
