#!/bin/bash
#
# For Theano configuration options see:
#
# http://deeplearning.net/software/theano/library/config.html#config.warn_float64
#
set -e
set -o pipefail

#python -c 'import theano; print(theano.config)' | less

THEANO_FLAGS='warn_float64=raise' python test_autoencoder.py
