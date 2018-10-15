# Lab6R

[![Build Status](https://travis-ci.org/DucDuong92/Lab6R.svg?branch=master)](https://travis-ci.org/DucDuong92/Lab6R)

## Descriptions:
This is the package for lab 6 of course Advanced R Programming This package contain knapsack objects and three functions:

* Brute_force_knapsack function: Use brute force approach to solve the knapsack problem. This function has an option for parallel processing by using multi-cores. The result is a list that contains the maximum value and the list of elements chosen.
* Dynamic_knapsack function: The same with the previous function, but use the dynamic approach to solve the problem
* Greedy_knapsack: The same with the first function, but use the greedy approach to solve the problem

## How to use the package:

````
devtools::install_github('DucDuong92/Lab6R')
data("knapsack_objects")
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500, parallel = TRUE)
dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)

````
