#!/usr/bin/env bash

set -e

function build_openapi2proto() {
  apt update
  apt install wget build-essential libz-dev zlib1g-dev -y

  VERSION="20.0.0"

  wget "https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-${VERSION}/graalvm-ce-java11-linux-amd64-20.0.0.tar.gz"

  tar -xzf "graalvm-ce-java11-linux-amd64-${VERSION}.tar.gz"

  export PATH="/graalvm-ce-java11-${VERSION}/bin:${PATH}"

  export JAVA_HOME="/graalvm-ce-java11-${VERSION}"

  gu install native-image

  cd openapi2proto || return

  rm -rf target/

  ./sbt "graalvm-native-image:packageBin"
}

build_openapi2proto
