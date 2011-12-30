#!/bin/bash

ROOT=$(cd $(dirname $0); pwd)

erlc -o $ROOT/ebin -I $ROOT/include $ROOT/src/pathlib.erl # needed by erlog
erlc -o $ROOT/ebin -I $ROOT/include $ROOT/src/erlog.erl

erlc -pz $ROOT/ebin -o $ROOT/ebin -I $ROOT/include $ROOT/src/*.erl

erlc -pz $ROOT/ebin -o $ROOT/ebin -I $ROOT/include $ROOT/test/*.erl
