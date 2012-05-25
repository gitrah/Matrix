#!/bin/bash
env JAVACMD=java JAVA_OPTS="-Xms2G -Xmx4G -enableassertions" scala -deprecation -classpath \
tools/log4j-1.2.15.jar:\
tools/junit-4.8.1.jar:\
tools/aspectjrt.jar:\
build/Matrix-unwoven.jar $@


  
