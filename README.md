# openapi2proto

Protobuf v3 with gRPC generator from subset of openapi v3

## Usage

```shell script
openapi2proto v0.1-beta.2
Usage: openapi2proto [compile|server] <args>...

Command: compile [options] <file>
compile openapi v3 to protobuf v3 with gRPC
  <file>                   openapi file in yaml format
  -t, --target-path <value>
                           output path
Command: server [options]
run web server with ui and rest api
  -p, --port <value>       port listen
  -h, --host <value>       host listen
```

## Install GraalVM

### MacOS

See details at [graalvm/homebrew-tap](https://github.com/graalvm/homebrew-tap)

```shell script
brew cask install graalvm/tap/graalvm-ce-java11
```

May be required to disable gatekeeper
```shell script
sudo spctl --master-disable
```

Add it to path

```shell script
export JAVA_HOME=/Library/Java/JavaVirtualMachines/graalvm-ce-java11-20.0.0/Contents/Home
export PATH="/Library/Java/JavaVirtualMachines/graalvm-ce-java11-20.0.0/Contents/Home/bin:$PATH"
```

## Install native-image

```shell script
gu install native-image
```

## Build project

```shell script
# clone project
git clone git@github.com:mitallast/openapi2proto.git
cd ./openapi2proto

# build project
./sbt "graalvm-native-image:packageBin"

# run application for test
./target/graalvm-native-image/openapi2proto
```