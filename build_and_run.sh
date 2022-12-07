#!/bin/bash
var=`docker container ls  | grep 'my-shiny-app' | awk '{print $1}'`
docker stop $var
go build tcp.go
docker build -t my-shiny-app .
docker run --net=host --rm -p 3838:3838 my-shiny-app 

# WARNING: Published ports are discarded when using host network mode
# adding --net=host

# acess docker as root:
# docker exec -u 0 -it a84a9e6c7ff8  bash