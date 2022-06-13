#! /bin/bash


CONFIG_FILEPATH="./new_sample.cfg" stack build --force-dirty --ghc-options -O --ghc-options -fforce-recomp && stack exec rupaka-exe -- "./output.json"
