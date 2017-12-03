#!/bin/sh
set -e

stack new "$1" template.hsfiles
cp stack.yaml.tmpl "$1/stack.yaml"
cp Makefile.tmpl "$1/Makefile"
cd $1
stack setup
