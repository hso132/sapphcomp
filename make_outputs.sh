#!/bin/bash

for file in `ls examples`
do
	java -cp amyc_2.12-1.5.jar amyc.Main --printTrees examples/$file >  test/resources/parser/outputs/"${file%%.*}.txt"
done
