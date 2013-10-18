#!/bin/sh

NODE=${1:-"release_manager"}
BIN="rels/$NODE/release/bin/release"

$BIN attach

