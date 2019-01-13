#! /usr/bin/env bash

install_stack()
{
  curl -sSL https://get.haskellstack.org/ | sh
}

install_psm()
{
  stack install postgresql-simple-migration
}


if [ -z $(which stack) ]
then echo "installing stack" && install_stack && install_psm
else echo "stack already setup"
fi
