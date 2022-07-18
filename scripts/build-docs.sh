#!/bin/bash -e
mkdir -p docs/servant-oauth2
mkdir -p docs/servant-oauth2-examples

pname=`cat package.yaml | grep "name:" | awk '{ print $2 }'`
ver=`cat package.yaml | grep "version:" | awk '{ print $2 }'`
stack haddock
doc_root=`stack path --local-doc-root`/$pname-$ver

cp -r $doc_root/* docs/servant-oauth2


cd servant-oauth2-examples
pname=`cat package.yaml | grep "name:" | awk '{ print $2 }'`
ver=`cat package.yaml | grep "version:" | awk '{ print $2 }'`
stack haddock
doc_root=`stack path --local-doc-root`/$pname-$ver

cp -r $doc_root/* ../docs/servant-oauth2-examples
