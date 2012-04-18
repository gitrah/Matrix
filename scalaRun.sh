#!/bin/bash
env JAVACMD=java JAVA_OPTS="-Xms512m -Xmx2048m -enableassertions" scala -deprecation -classpath \
./:./classes:\
$M2_REPO/log4j/log4j/1.2.14/log4j-1.2.14.jar:\
$M2_REPO/javax/persistence/persistence-api/1.0/persistence-api-1.0.jar:\
$M2_REPO/commons-httpclient/commons-httpclient/3.1/commons-httpclient-3.1.jar:\
$M2_REPO/commons-logging/commons-logging/1.1.1/commons-logging-1.1.1.jar:\
$M2_REPO/commons-codec/commons-codec/1.3/commons-codec-1.3.jar:\
$M2_REPO/junit/junit/4.8.1/junit-4.8.1.jar $@

  
