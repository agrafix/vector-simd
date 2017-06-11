#!/bin/bash
rsync -rtvu --exclude '.git' --exclude '.stack-work' . priv-beast:devel/vector-simd/
