# Revisiting Optimal Proportions for Binary Responses: Insights from Incorporating the Absent Perspective of Type-I Error Rate Control

This repository contains the R code necessary to replicate the results presented in the paper *"Revisiting Optimal Proportions for Binary Responses: Insights from Incorporating the Absent Perspective of Type-I Error Rate Control"* by Lukas Pin, Sofía S. Villar and William F. Rosenberger. The code explores optimal proportions in binary response settings, incorporating considerations of Type-I error rate control.

## About the Paper

The paper investigates the optimal allocation proportions for binary response outcomes while addressing the often-overlooked impact of Type-I error rate control. The study provides insights into improving statistical efficiency and reliability in hypothesis testing.

## Code Structure

The code can be executed using parallelisation or in a sequential manner, depending on available computational resources.

The main simulation function is contained in `Simulation_ErrorRate.R`, which orchestrates the entire simulation process by integrating several subfiles:

- **`ER.R`** - Implements equal randomization methodology.
- **`ERADE.R`** - Computes optimal proportions for error rate control.
- **`WaldTest.R`** - Conducts statistical testing using the Wald test.
- **`Data_Generator.R`** - Generates simulated datasets for evaluation.

## Prerequisites

Ensure that R is installed on your system before running the simulation code. Additional R packages may be required depending on the specific implementation details.

## Author

This code has been developed by Lukas Pin.

For inquiries, contact: **lukas.pin@mrc-bsu.cam.ac.uk**

