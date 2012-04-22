#!/bin/sh
if test -f ~/.sbtconfig; then
  . ~/.sbtconfig
fi
exec java -Xmx12000M -XX:+UseConcMarkSweepGC -XX:MaxPermSize=128m ${SBT_OPTS} -jar sbt-launch-0.11.2.jar "$@"
