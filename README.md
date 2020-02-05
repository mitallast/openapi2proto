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
# instal for java8
brew cask install graalvm/tap/graalvm-ce-java8
# or java11
brew cask install graalvm/tap/graalvm-ce-java11
```

May be required to disable gatekeeper
```shell script
sudo spctl --master-disable
```

Add it to path

```shell script
export JAVA_HOME=/Library/Java/JavaVirtualMachines/graalvm-ce-java11-19.3.1/Contents/Home
export PATH="$PATH":/Library/Java/JavaVirtualMachines/graalvm-ce-java11-19.3.1/Contents/Home/bin
```

## Install native-image

```shell script
gu install native-image
```

## Build tool

```shell script
sbt "graalvm-native-image:packageBin"
```