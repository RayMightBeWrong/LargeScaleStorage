#!/bin/bash

# Function to compile Java files
compile_java() {
    find . -name "*.java" > sources.txt
    mkdir -p out/java
    javac -cp .:lib/jeromq-0.5.3.jar -d out/java @sources.txt
    rm sources.txt
}

# Function to compile Erlang files
compile_erlang() {
    mkdir -p out/erlang
    erlc -o out/erlang ./SessionServer/*.erl ./SessionServer/CRDTs/*.erl ./SessionServer/Actors/*.erl ./SessionServer/AuxiliaryModules/*.erl
}

# Compile Java files
compile_java

# Compile Erlang files
compile_erlang
