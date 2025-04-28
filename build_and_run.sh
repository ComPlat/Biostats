#!/bin/bash

sudo systemctl stop shiny-server

# Stop any running biostats-app container
var=$(docker container ls | grep 'biostats-app' | awk '{print $1}')
docker stop $var

# Build the Docker image
docker build -t biostats-app .

# Run the container
docker run --net=host --rm -p 3838:3838 biostats-app

