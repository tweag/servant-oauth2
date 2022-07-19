#!/bin/bash -e
mkdir -p docs/servant-oauth2
mkdir -p docs/servant-oauth2-examples

# Note: We run stack twice, the second time it just tells us the paths; we
# can't do better because the world is a frustrating place.
stack haddock --no-haddock-deps
index=`stack haddock --no-haddock-deps 2>&1 | sed -n 2p`

ver=`cat package.yaml | grep "version:" | awk '{ print $2 }'`
exVer=`cat servant-oauth2-examples/package.yaml | grep "version:" | awk '{ print $2 }'`

doc_root=$(dirname "$index")

cp -r $doc_root/servant-oauth2-$ver/* docs/servant-oauth2
cp -r $doc_root/servant-oauth2-examples-$exVer/* docs/servant-oauth2-examples

# Elegant method of making the links correct ...
# Note: lts-19.14 is hard-coded here. Could locate it from the `stack.yaml` if
# we wished it.
sed -i 's/href=\"\.\./\href="\https:\/\/www\.stackage\.org\/haddock\/lts-19\.14/g' docs/**/*.html
