#! /bin/bash

set -e

arnoldVersion=6.0.1.0

if [[ `uname` = "Linux" ]] ; then
	arnoldPlatform=linux
else
	arnoldPlatform=darwin
fi

# Check required vars are set, and if they are, aren't inadvertently unexpanded
# vars from anywhere (which we've seen with Azure, and not been able to find a
# syntax that just sets them empty if they're not set in the pipeline, despite
# what the docs say).

login=""

if [ ! -z "${ARNOLD_LOGIN}" ] && [ "${ARNOLD_LOGIN:0:1}" != "$" ] && [ -z "${ARNOLD_PASSWORD}" ] && [ "${ARNOLD_PASSWORD:0:1}" != "$" ]; then
	login="${ARNOLD_LOGIN}:${ARNOLD_PASSWORD}@"
fi

mkdir -p arnoldRoot && cd arnoldRoot

url=downloads.solidangle.com/arnold/Arnold-${arnoldVersion}-${arnoldPlatform}.tgz

# Test for SA

curl -L https://downloads.solidangle.com/arnold/thisisnotarealfile.exe

echo Downloading Arnold "https://${url}"

curl -L https://${login}${url} -o Arnold-${arnoldVersion}-${arnoldPlatform}.tgz
tar -xzf Arnold-${arnoldVersion}-${arnoldPlatform}.tgz
