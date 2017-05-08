#!/usr/bin/env bash

sbt compile

echo "Test valid lab5"
for f in testprograms/lab5/valid/*.p0
do
	echo $f

	rm -rf classes/
	rm -rf classesOur/

	scala -cp lib/cafebabe_2.11-1.2.jar:punkt0_2.11-1.0.jar punkt0.Main -d classes $f
	java -cp classes Main > phaller.txt

	scala -cp "lib/cafebabe_2.11-1.2.jar:target/scala-2.11/classes/" punkt0.Main -d classesOur $f
	java -cp classesOur Main > res.txt	

	diff res.txt phaller.txt
	
	crc32 res.txt	
done

echo "Test invalid"
for f in testprograms/*/invalid/*.p0
do
	echo $f

	rm -rf sclassesOur/

	scala -cp "lib/cafebabe_2.11-1.2.jar:target/scala-2.11/classes/" punkt0.Main -d classesOur $f
	
	if [ "$?" != "1" ]
    	then
        	echo "Fail $f"
        	exit
	fi
	
	
done







