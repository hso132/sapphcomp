#!/bin/bash

libraries="Nil"
for lib in `ls library`
do
	lib=${lib%%.*}
	libraries="\"$lib\" :: $libraries"
done
echo "val libs: List[String] = $libraries;"
for file in `ls examples`
do
	fileName=${file%%.*}
	cat examples/$file > test/resources/names/passing/$file
	java -cp amyc_2.12-1.5.jar amyc.Main --printNames library/* examples/$file >  test/resources/names/outputs/"$fileName.txt"
	echo "@Test def test$fileName = shouldOutput(\"$fileName\" :: libs, \"$fileName\");"
done
