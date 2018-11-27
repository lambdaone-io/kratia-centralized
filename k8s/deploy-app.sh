#!/bin/bash

set -x
script_path=$(dirname "$0")

REGISTRY=registry.gitlab.com/kolov
APP_NAME=concierge

sed -e s@%VERSION%@master@g \
    -e s@%APP_NAME%@$APP_NAME@g \
    -e s@%REGISTRY%@$REGISTRY@g \
    $script_path/k8s-deployment.yml > $script_path/k8s-deployment-master.yml
kubectl apply -f $script_path/k8s-deployment-master.yml
kubectl apply -f $script_path/k8s-service.yml