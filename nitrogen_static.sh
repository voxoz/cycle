#!/bin/bash

FILES=apps/releaseman/priv/static/nitrogen
rm -rf $FILES
ln -s ../../../../deps/n2o/priv/static/n2o $FILES
