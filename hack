#!/bin/bash
set -ex
fname=$1
cmd="bash -c \"pkill ${fname}; stack exec ${fname} &\""
stack build --fast --file-watch --exec "${cmd}"
