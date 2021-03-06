#!/usr/bin/env bash

################################################################################
#
# Run this script if you would like to know the versions of
# bcc-wallet and related programs installed in the path.
#
# Run it inside a nix-shell to see what program versions would be used
# by CI:
#   nix-shell --pure --run "./scripts/program-revs.sh"
#
# It can also be used for inspecting versions included in a release
# package:
#   tar -C /tmp -xzvf ~/Downloads/bcc-wallet-v2020-11-17-linux64.tar.gz
#   PATH=/tmp/bcc-wallet-linux64:$PATH ./scripts/program-revs.sh
#
################################################################################

set -euo pipefail

for prog in bcc-wallet bcc-node bcc-cli bcc-address bech32; do
  echo
  if type -p $prog > /dev/null; then
    echo "Your $prog is $(type -p $prog)"
    if [ $prog = bcc-wallet ]; then
      $prog version
    elif [ $prog = bech32 ]; then
      echo "no version info"
    else
      $prog --version
    fi
  else
    echo "You don't have $prog in your PATH."
  fi
done
