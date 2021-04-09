#!/bin/bash

NAME="${1-list}"

if [ "$NAME" = "list" ]; then
  aws ec2 describe-instances --filters "Name=instance-state-name,Values=running" --query "Reservations[].Instances[?not_null(Tags[?Key=='Name'].Value)]|[].[Tags[?Key=='Name'].Value|[0],InstanceId]" --output=text

else
  INSTANCE_ID=$(aws ec2 describe-instances --filters "Name=tag:Name,Values=$NAME" --query 'Reservations[*].Instances[*].[InstanceId]' --output=text)

  echo "Connecting to $NAME via $INSTANCE_ID"
  aws ssm start-session --target "$INSTANCE_ID"
fi
