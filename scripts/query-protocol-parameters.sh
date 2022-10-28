#!/bin/bash

mkdir -p temp/$BLOCKCHAIN_PREFIX

cardano-cli query protocol-parameters \
    $BLOCKCHAIN \
    --out-file "temp/$BLOCKCHAIN_PREFIX/protocol-parameters.json"
