#!/bin/sh
if test -f ~/.sbtconfig; then
  . ~/.sbtconfig
fi
java -Xms512M -Xmx3072M -Xss1M -XX:+CMSClassUnloadingEnabled -jar `dirname $0`/sbt-launch.jar "$@"
