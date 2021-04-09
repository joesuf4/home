#!/bin/bash

. ~/.bcsrc
. ~/.ec2rc

if [ $# = 0 ]; then
  aws ec2 describe-instances --filters "Name=instance-state-name,Values=running" --query "Reservations[].Instances[?not_null(Tags[?Key=='Name'].Value)]|[].[Tags[?Key=='Name'].Value|[0],InstanceId]" --output=text

else
  INSTANCE_ID="${EC2_ID[$1]}"

  echo "Connecting to $NAME via $INSTANCE_ID"
  aws ssm start-session --target "$INSTANCE_ID"
fi
