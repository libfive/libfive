#!/bin/bash
set -x -e

EXE=Studio
APP=gui/$EXE.app

VERSION=`git describe --exact-match --tags || echo "($(git rev-parse --abbrev-ref HEAD))"`
VERSION=`echo $VERSION|sed s:/:-:g`

cd ../../../build
rm -rf $APP
ninja clean
ninja

# Pull out framework paths info with otool
MACDEPLOYQT=`otool -L $APP/Contents/MacOS/$EXE | sed -n -e "s:\(.*\)lib/QtCore.*:\1/bin/macdeployqt:gp"`

$MACDEPLOYQT $APP

# Delete unused Qt plugins
cd $APP/Contents/PlugIns
rm -rf accessible audio imageformats mediaservice playlistformats position printsupport qml1tooling sensorgestures sensors bearer

fix_qt () {
    echo "Fixing Qt for $1"
    for LIB in $( otool -L $1 | sed -n -e "s:\(.*Qt.*.framework[^ ]*\).*:\1:gp" )
    do
        RENAMED=`echo $LIB | sed -n -e "s:.*\(Qt.*\)\.framework.*:@executable_path/../Frameworks/\1.framework/Versions/5/\1:gp"`
        install_name_tool -change $LIB $RENAMED $1
    done
}

# Remap platform links
cd platforms
fix_qt libqcocoa.dylib

# Delete unused Qt frameworks
cd ../../Frameworks
rm -rf QtDeclarative.framework QtMultimedia.framework QtMultimediaWidgets.framework QtPositioning.framework QtQml.framework QtQuick.framework QtScript.framework QtSensors.framework QtSql.framework QtXmlPatterns.framework Qt3DCore.framework Qt3DRender.framework QtLocation.framework QtSerialBus.framework QtSerialPort.framework

# Clean up remaining Qt frameworks
for LIB in $( ls|sed -n -e "s:\(Qt.*\)\.framework:\1:gp" )
do
    fix_qt $LIB.framework/Versions/Current/$LIB
done

# Update release number in Info.plist
cd ../../../..
cp ../gui/deploy/mac/Info.plist $APP/Contents/Info.plist
sed -i "" "s:0\.0\.0:$VERSION:g" $APP/Contents/Info.plist

# Build icon and deploy into bundle
convert -background none ../gui/deploy/icon/icon.svg icon.png
png2icns ao.icns icon.png
mv ao.icns $APP/Contents/Resources/ao.icns
rm icon.png

# Create the disk image
rm -rf $EXE $EXE.dmg
mkdir $EXE
cp ../README.md ./$EXE/README.txt
cp -R $APP ./$EXE
hdiutil create $EXE.dmg -volname "$EXE $VERSION" -srcfolder $EXE
rm -rf $EXE
mv $EXE.dmg ..
