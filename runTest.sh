#!/usr/bin/env bash

for f in testprograms/lab3/valid/*.p0
do
    AST=${f}.ast
	echo "$f -> $AST"
	sbt "run --ast $f" > /dev/null 2> res.p0.ast
	DIFF=$(diff res.p0.ast $AST)
	if [ "$DIFF" != "" ]
	then
	    echo "Fail $f"
	    diff res.p0.ast $AST
	    exit
	fi
done