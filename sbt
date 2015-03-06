#!/bin/sh
if test -f ~/.sbtconfig; then
  . ~/.sbtconfig
fi
java -Xms512M -Xmx3072M -Xss1M -XX:PermSize=128M -XX:MaxPermSize=512M -XX:+CMSClassUnloadingEnabled -jar `dirname $0`/sbt-launch-0.13.1.jar "$@"
