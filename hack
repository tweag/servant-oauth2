#!/bin/bash
set -ex
stack build --fast --file-watch --exec 'bash -c "pkill example; stack exec example &"'
