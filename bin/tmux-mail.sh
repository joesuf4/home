#!/bin/bash

[[ "$(stat -c %s /var/spool/mail/$USER)" -gt 600 ]] && echo "<Mail>"
