#!/usr/bin/env bash

#http://www.csc.kth.se/~phaller/compilers/labs/punkt0_2.11-1.2.jar

sbt compile

echo "Test valid lab5"
for f in testprograms/lab5/valid/*.p0
do
	echo $f

	rm -rf classes/
	rm -rf classesOur/

	#Reference compiler
	scala -cp lib/cafebabe_2.11-1.2.jar:punkt0_2.11-1.0.jar punkt0.Main -d classes $f
	
	if [ "$?" != "0" ]
    	then
        	echo "This is not a valid program"
        	exit
	fi

	java -cp classes Main > phaller.txt

	#Our compiler
	scala -cp "lib/cafebabe_2.11-1.2.jar:target/scala-2.11/classes/" punkt0.Main -d classesOur $f

	if [ "$?" != "0" ]
    	then
        	echo "Fail $f"
        	exit
	fi	

	java -cp classesOur Main > res.txt	

	#Diff
	diff res.txt phaller.txt
	
	crc32 res.txt	
done

echo "Test invalid"
for f in testprograms/*/invalid/*.p0
do
	echo $f

	rm -rf sclassesOur/
	rm -rf classes/

	#Reference compiler

	scala -cp lib/cafebabe_2.11-1.2.jar:punkt0_2.11-1.0.jar punkt0.Main -d classes $f

	if [ "$?" != "1" ]
    	then
        	echo "This is not an unvalid program $f"
        	exit
	fi

	#Our compiler
	
	scala -cp "lib/cafebabe_2.11-1.2.jar:target/scala-2.11/classes/" punkt0.Main -d classesOur $f
	
	if [ "$?" != "1" ]
    	then
        	echo "Fail $f"
        	exit
	fi
	
	
done







