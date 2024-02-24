#!/usr/bin/bash

echo "# Generated from *.rcl by generate.sh, do not edit manually." | tee build.yml deploy.yml
rcl evaluate --format=json build.rcl >> build.yml
rcl evaluate --format=json deploy.rcl >> deploy.yml
