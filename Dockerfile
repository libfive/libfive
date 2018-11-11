FROM ubuntu:latest
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y cmake pkg-config libeigen3-dev libpng-dev libboost-all-dev qtbase5-dev guile-2.2-dev
RUN apt-get install -y clang
ADD . /src
RUN mkdir /build
WORKDIR /build
# for clang use: docker build . --build-arg CC=clang --build-arg CXX=clang++
ARG CC=gcc
ARG CXX=g++
RUN cmake /src && make -j4
