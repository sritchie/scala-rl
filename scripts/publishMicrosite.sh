#!/bin/bash
set -e

git config --global user.email "sritchie09@gmail.com"
git config --global user.name "Sam Ritchie"
git config --global push.default simple

sbt docs/publishMicrosite
