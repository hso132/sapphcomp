#!/bin/bash

libraries="Nil"
outdir="test/resources/types/passing/"
for lib in `ls library`
do
	cp library/$lib -t $outdir
	lib=${lib%%.*}
	libraries="\"$lib\" :: $libraries"
done
echo "val libs: List[String] = $libraries;"
for file in `ls examples`
do
	fileName=${file%%.*}
	cat examples/$file > "$outdir$file"
	#java -cp amyc_2.12-1.5.jar amyc.Main --printNames library/Std.scala library/Option.scala library/List.scala examples/$file >  test/resources/names/outputs/"$fileName.txt"
	echo "@Test def test$fileName = shouldPass(libs :+ \"$fileName\", \"$fileName\");"
done
