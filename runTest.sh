#!/usr/bin/env bash

#echo "Test valid lab2"
#for f in testprograms/lab2/valid/*.p0
#do
#    CHECK=${f%.*}.check
#    echo "$f -> $CHECK"
#    sbt "run --tokens $f" > /dev/null 2> res.p0.ast
#    DIFF=$(diff res.p0.ast $CHECK)
#    crc32 res.p0.ast
#    head -c 70 res.p0.ast
#    echo ""
#    if [ "$DIFF" != "" ]
#    then

#        echo "Fail $f"
#        diff res.p0.ast $CHECK
#        exit
#    fi
#done

echo "Test valid pretty-print lab3"
for f in testprograms/lab3/valid/*.p0
do
    AST=${f}.ast
    echo "$f -> $AST"
    sbt "run --print $f" > /dev/null 2> res.p0
    sbt "run --ast res.p0" > /dev/null 2> res.p0.ast
    DIFF=$(diff res.p0.ast $AST)
    crc32 res.p0.ast
    head -c 90 res.p0.ast
    echo ""
    if [ "$DIFF" != "" ]
    then

        echo "Fail $f"
        diff res.p0.ast $AST
        exit
    fi
done

echo "Test invalid lab3"
for f in testprograms/lab3/invalid/*.p0
do
    echo "$f"
    sbt "run --ast $f" > /dev/null 2> res.p0.ast


    if [ "$?" == "0" ]
    then
        echo "Fail $f"
        exit
    fi
    crc32 res.p0.ast
    head -c 100 res.p0.ast
    echo ""
done

echo "Test valid lab3"
for f in testprograms/lab3/valid/*.p0
do
    AST=${f}.ast
    echo "$f -> $AST"
    sbt "run --ast $f" > /dev/null 2> res.p0.ast
    DIFF=$(diff res.p0.ast $AST)
    crc32 res.p0.ast
    head -c 90 res.p0.ast
    echo ""
    if [ "$DIFF" != "" ]
    then

        echo "Fail $f"
        diff res.p0.ast $AST
        exit
    fi
done


