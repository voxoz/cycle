#!/bin/bash

NODE=${1:-"release_manager"}

function release_node {
    rm -rf rels/$1/node/lib
    rm -rf rels/$1/node/log
    rm -rf rels/$1/node/releases
    cd rels/$1
    rebar -f generate
    cp -r /usr/lib/erlang/lib/erl_interface-* release/lib
    cd ../..
}

if [ "$NODE" == "all" ]; then
   echo "Releasing all nodes..."
   release_node release_manager
else
   echo "Releasing node $NODE..."
   release_node $NODE
fi

echo $?
