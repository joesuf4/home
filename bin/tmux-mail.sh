#!/bin/bash
messages="$(grep -c "^From " /var/spool/mail/$USER 2>/dev/null)"

if [[ "$messages" -gt 0 ]]; then
  echo "Mail($messages)"
fi
