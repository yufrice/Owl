#!/bin/bash

set -eu

cd /opt/yesod

/usr/local/stack exec -- yesod devel
