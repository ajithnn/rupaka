#!/bin/bash

val=0
while [ $val -le 5 ]
do
	cmd=$(echo "stack exec rupaka -- -o examples/outputs-$val.json -c examples/example-$val.cfg -v examples/example-$val.vld -s")
	echo "Running: $cmd"
	$cmd
	val=$(( $val + 1 ))
done

