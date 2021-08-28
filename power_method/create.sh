#!/bin/bash
gfortran iteration_method.f90 -c
gfortran main.f90 iteration_method.o -o pother_method_0.2
