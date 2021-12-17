#!/bin/bash

[[ "$(mailcheck|wc -l)" -gt 0 ]] && echo "<Mail>"
