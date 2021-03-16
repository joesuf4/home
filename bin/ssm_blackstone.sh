#!/bin/bash
# Connect via SSM to Blackstone Instances by tag:hostname

: ${AWS_PROFILE:=prod-us-east-1}
HOSTNAME="${1-list}"

if [ "$HOSTNAME" = "list" ]; then
  aws ec2 describe-instances --profile=$AWS_PROFILE --filters "Name=instance-state-name,Values=running" --query "Reservations[].Instances[?not_null(Tags[?Key=='hostname'].Value)]|[].[Tags[?Key=='hostname'].Value|[0],InstanceId]" --output=text

else
  INSTANCE_ID=$(aws ec2 describe-instances --profile=$AWS_PROFILE --filters "Name=tag:hostname,Values=$HOSTNAME" --query 'Reservations[*].Instances[*].[InstanceId]' --output=text)

  echo "Connecting to $HOSTNAME via $INSTANCE_ID"
  aws ssm --profile=$AWS_PROFILE start-session --target $INSTANCE_ID
fi
