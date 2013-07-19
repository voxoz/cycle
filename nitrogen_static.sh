#!/bin/bash

N2O=deps/n2o/priv/static
FILES=apps/releaseman/priv/static/nitrogen
rm -rf $N2O
rm -rf $FILES
ln -s ../../n2o_scripts $N2O
ln -s ../../../../deps/n2o/priv/static/n2o $FILES
