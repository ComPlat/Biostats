#!/bin/bash

docker build -t konradkraemer/biostats:latest .
# docker build --no-cache -t konradkraemer/biostats:latest .

docker push konradkraemer/biostats:latest
