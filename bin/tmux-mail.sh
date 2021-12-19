#!/bin/bash
messages="$(grep -c "^From " /var/spool/mail/$USER 2>/dev/null)"
messages_read="$(perl -l -000 -ne 'BEGIN{$count=0} $count++ if m/^From / and /^Status: RO/m; END {print $count}' /var/spool/mail/$USER 2>/dev/null)"

if [[ "$messages" -gt "$messages_read" ]]; then
  echo "Mail($((messages - messages_read)))"
fi
