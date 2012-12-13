#!/bin/bash
env JAVACMD=java JAVA_OPTS="-Dprofiling=true -Xms2G -Xmx4G -enableassertions -Xdebug -Xrunjdwp:transport=dt_socket,address=1773,server=y,suspend=n" scala -deprecation -classpath \
test-classes:\
tools/sgt_v30.jar:\
tools/log4j-1.2.15.jar:\
tools/junit-4.8.1.jar:\
tools/aspectjrt.jar:\
build/Matrix-unwoven.jar $@

  
