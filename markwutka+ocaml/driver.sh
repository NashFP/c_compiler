#!/bin/sh
set -x
MODE=""
if [ "$1" = "--lex" ]; then
  MODE="--lex"
  shift
elif [ "$1" = "--parse" ]; then
  MODE="--parse"
  shift
elif [ "$1" = "--codegen" ]; then
  MODE="--codegen"
  shift
fi
SOURCE_FILE=$1
BASE=${SOURCE_FILE%.*}
PREPROCESSED=$BASE".i"
ASSEMBLED=$BASE".s"
ROOT_DIR=`dirname "$0"`
gcc -E $SOURCE_FILE -o $PREPROCESSED
ERR=$?
if [ $ERR -ne 0 ]; then
  rm $PREPROCESSED
  exit $ERR
fi
cat $PREPROCESSED
$ROOT_DIR/_build/default/bin/main.exe $MODE $PREPROCESSED
ERR=$?
rm $PREPROCESSED
if [ $ERR -ne 0 ]; then
  exit $ERR
fi
if [ "$MODE" = "" ]; then
  gcc $ASSEMBLED -o $BASE
  ERR=$?
  rm $ASSEMBLED
  exit $ERR
fi

