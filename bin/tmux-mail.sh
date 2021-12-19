#!/bin/bash
messages="$(grep -c "^From " /var/spool/mail/$USER 2>/dev/null)"
messages_read="$(grep -c "^Status: R" /var/spool/mail/$USER 2>/dev/null)"

if [[ "$messages" -gt "$messages_read" ]]; then
  messages=$((messages - messages_read))
  echo "Mail($messages)"
fi
