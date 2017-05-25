#!/bin/bash

# Creating Scala documentation:

scaladoc -d doc src/rete/*.scala src/rete/graphviz/*.scala

# Creating jar file:

cd bin
jar -cfv logfire.jar rete
cd ..

# Package doc and jar file into one zip file:

mkdir logfirelib
cp -r doc logfirelib/doc
mv bin/logfire.jar logfirelib

rm logfirelib.zip
zip -r logfirelib.zip logfirelib

