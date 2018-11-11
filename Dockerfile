FROM ubuntu:latest
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y cmake pkg-config libeigen3-dev libpng-dev libboost-all-dev qtbase5-dev guile-2.2-dev
ADD . /src
RUN mkdir /build
WORKDIR /build
RUN cmake /src && make -j4