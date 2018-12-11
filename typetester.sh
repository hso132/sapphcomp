#!/bin/bash

declare -a types=("()" "true" "H()" "3")
outdir="test/resources/types/failing/"
i=0
for type1 in ${types[@]}
do
	for type2 in ${types[@]}
	do
		filename="$type1$type2"
		fullFilename="$outdir$filename.scala"
		echo "object types {
		abstract class J
		case class H() extends J" > $fullFilename
		echo "$type1 == $type2}" >> $fullFilename
		echo "@Test def test$i = shouldFail(\"$filename\")"
		let i=$i+1
	done
done
