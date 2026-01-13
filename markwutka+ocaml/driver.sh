#!/bin/sh

# According to the book, if no option is specified, the
# code should be compiled. This script sets the mode to
# --compile if none of the ones in the book are used.
MODE="--compile"
if [ "$1" = "--lex" ]; then
  MODE="--lex"
  shift
elif [ "$1" = "--parse" ]; then
  MODE="--parse"
  shift
elif [ "$1" = "--codegen" ]; then
  MODE="--codegen"
  shift
elif [ "$1" = "-S" ]; then
  MODE="-S"
  shift
fi
SOURCE_FILE=$1
BASE=${SOURCE_FILE%.*}
PREPROCESSED=$BASE".i"
ASSEMBLED=$BASE".s"
ROOT_DIR=`dirname "$0"`

# Run the C preprocessor on the file
gcc -E $SOURCE_FILE -o $PREPROCESSED

# Exit if there is an error
ERR=$?
if [ $ERR -ne 0 ]; then
  rm $PREPROCESSED
  exit $ERR
fi

# Execute my C compiler, change this line to execute yours
$ROOT_DIR/_build/default/bin/main.exe $MODE $PREPROCESSED

# Exit if there was an error
ERR=$?
rm $PREPROCESSED
if [ $ERR -ne 0 ]; then
  exit $ERR
fi

if [ "$MODE" = "--compile" ]; then
  gcc $ASSEMBLED -o $BASE
  ERR=$?
  rm $ASSEMBLED
  exit $ERR
fi
