#!/bin/bash

for file in `ls examples`
do
	java -cp amyc_2.12-1.5.jar amyc.Main --printTokens examples/$file >  test/resources/lexer/outputs/"${file%%.*}.txt"
done
