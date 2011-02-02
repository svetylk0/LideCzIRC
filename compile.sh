#!/bin/bash

if [ ! -d build ]; then
  mkdir build
fi

fsc -d build src/*.scala
