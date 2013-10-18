#!/bin/bash

wd=`pwd`
appss=("rels/release_manager/release/lib/n2o-3.0"
       "rels/release_manager/release/lib/releaseman-1" )

declare -A sources_apps
declare -A sources_deps
declare -A apps

for src in `ls -d apps/*`; do 
    app=(`echo $src | tr '/' ' '`)
    sources_apps[${app[1]}]="1"
done

for src in `ls -d deps/*`; do 
    app=(`echo $src | tr '/' ' '`)
    sources_deps[${app[1]}]="1"
done

for dir in ${appss[@]} ; do
    lib=(`echo $dir | tr '-' ' ' | tr '/' ' '`)
    ap=${lib[4]}
    if [ "${sources_apps[$ap]}" = "1" ]; then
        apps[$dir]="apps/$ap"
    fi
    if [ "${sources_deps[$ap]}" = "1" ]; then
        apps[$dir]="deps/$ap"
    fi
done

for key in ${!apps[@]}; do 
    echo "release sync processing $key -> ${apps[$key]}";
    rm -rf "$key/ebin"
    rm -rf "$key/include"
    rm -rf "$key/priv"
    if [ -d "${apps[$key]}/ebin" ]; then
         ln -s  "$wd/${apps[$key]}/ebin" "$wd/$key/ebin"
    fi
    if [ -d "${apps[$key]}/include" ]; then
         ln -s  "$wd/${apps[$key]}/include" "$wd/$key/include"
    fi
    if [ -d "${apps[$key]}/priv" ]; then
         ln -s  "$wd/${apps[$key]}/priv" "$wd/$key/priv"
    fi
done

echo $?
