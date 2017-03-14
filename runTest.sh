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

echo "Compile"
sbt compile

echo "Test valid pretty-print lab3"
for f in testprograms/lab3/valid/*.p0
do
    AST=${f}.ast
    echo "$f -> $AST"
    scala "-cp target/scala-2.11/classes/ punkt0.Main --print $f" > res.p0
    scala "-cp target/scala-2.11/classes/ punkt0.Main --print res.p0" > ans.p0

    DIFF=$(diff res.p0 ans.p0 )
    crc32 ans.p0
    #head -c 90 ans.p0
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
    scala "-cp target/scala-2.11/classes/ punkt0.Main --ast $f" > res.p0.ast

    if [ "$?" != "1" ]
    then
        echo "Fail $f"
        exit
    fi
    crc32 res.p0.ast
    #head -c 100 res.p0.ast
    echo ""
done

echo "Test valid lab3"
for f in testprograms/lab3/valid/*.p0
do
    AST=${f}.ast
    echo "$f -> $AST"
    scala "-cp target/scala-2.11/classes/ punkt0.Main --ast $f" > res.p0.ast
    DIFF=$(diff res.p0.ast $AST)
    crc32 res.p0.ast
    #head -c 90 res.p0.ast
    echo ""
    if [ "$DIFF" != "" ]
    then

        echo "Fail $f"
        diff res.p0.ast $AST
        exit
    fi
done


