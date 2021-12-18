#!/bin/bash
messages="$(grep -c "^From " /var/spool/mail/$USER 2>/dev/null)"

if [[ "$messages" -gt 1 ]]; then
  ((--messages))
  echo "Mail($messages)"
fi
