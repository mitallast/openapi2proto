# openapi2proto

Protobuf v3 with gRPC generator from subset of openapi v3

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
export PATH=/Library/Java/JavaVirtualMachines/graalvm-ce-java11-19.3.0/Contents/Home/bin:"$PATH"
```

## Install native-image

```shell script
gu install native-image
```

## Build tool

```shell script
sbt "graalvm-native-image:packageBin"
```