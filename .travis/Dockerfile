FROM ubuntu:18.04

RUN apt-get update

RUN apt-get install -y -qq --no-install-recommends \
    g++ cmake pkg-config libeigen3-dev libpng-dev libboost-all-dev qtbase5-dev libqt5opengl5-dev guile-2.2-dev

COPY run-tests.sh /root/run-tests.sh

ENTRYPOINT ["/root/run-tests.sh"]
