FROM ubuntu:18.04

RUN apt-get update

RUN apt-get install -y -qq --no-install-recommends \
    g++ cmake pkg-config libeigen3-dev libpng-dev libboost-all-dev qtbase5-dev libqt5opengl5-dev guile-2.2-dev locales

RUN locale-gen en_US.UTF-8

ENTRYPOINT ["/src/.travis/run-tests.sh"]
