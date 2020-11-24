#!/bin/bash
# Connect via SSM to honorlock Instances by tag:hostname

PROFILE=honorlock
HOSTNAME="$1"

if [ "$HOSTNAME" = "list" ]; then
  echo "Listing SSM resources:"
  aws ec2 describe-instances --profile=$PROFILE --query "Reservations[].Instances[?not_null(Tags[?Key=='hostname'].Value)]|[].[Tags[?Key=='hostname'].Value|[0],InstanceId]" --output=table

else
  INSTANCE_ID=$(aws ec2 describe-instances --profile=$PROFILE --filters "Name=tag:hostname,Values=$HOSTNAME" --query 'Reservations[*].Instances[*].[InstanceId]' --output=text)

  echo "Connecting to $HOSTNAME via $INSTANCE_ID"
  aws ssm --profile=$PROFILE start-session --target $INSTANCE_ID
fi