#!/bin/bash
set -x -e

EXE=Studio
APP=$EXE.app

VERSION=`git describe --exact-match --tags || echo "($(git rev-parse --abbrev-ref HEAD))"`
VERSION=`echo $VERSION|sed s:/:-:g`

cd ../../..
rm -r build
mkdir build
cd build
cmake -DCMAKE_PREFIX_PATH=/usr/local/Cellar/qt/5.9.2 -GNinja -DCMAKE_OSX_DEPLOYMENT_TARGET=10.10 ..
rm -rf $APP gui/$APP
ninja clean
ninja

# Copy to a new location before modifying, so that the built app doesn't
# get modified and future builds don't misbehave due to duplicate frameworks
cp -r gui/$APP $APP

# Pull out framework paths info with otool
MACDEPLOYQT=`otool -L $APP/Contents/MacOS/$EXE | sed -n -e "s:\(.*\)lib/QtCore.*:\1/bin/macdeployqt:gp"`
GUILE_SCM=`otool -L $APP/Contents/MacOS/$EXE | sed -n -e "s:lib/libguile.*:share/guile/2.2/:gp"`
GUILE_CCACHE=`otool -L $APP/Contents/MacOS/$EXE | sed -n -e "s:lib/libguile.*:lib/guile/2.2/ccache/:gp"`

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

# Deploy Guile library (including both bare scm files and precompiled,
# on the assumption that stuff which is useful for this application
# will have been pre-compiled at least one)
cd ../Resources
mkdir -p guile/scm
mkdir -p guile/ccache
cp -r $GUILE_SCM guile/scm/
cp -r $GUILE_CCACHE guile/ccache/

# Update release number in Info.plist
cd ../../..
cp ../gui/deploy/mac/Info.plist $APP/Contents/Info.plist
sed -i "" "s:0\.0\.0:$VERSION:g" $APP/Contents/Info.plist

# Build icon and deploy into bundle
convert -background none ../gui/deploy/icon/icon.svg -resize 512x512 icon512.png
convert icon512.png -resize 256x256 icon256.png
convert icon512.png -resize 128x128 icon128.png
convert icon512.png -resize 32x32 icon32.png
convert icon512.png -resize 16x16 icon16.png
png2icns ao.icns icon512.png icon256.png icon128.png icon32.png icon16.png
mv ao.icns $APP/Contents/Resources/ao.icns
rm icon512.png icon256.png icon128.png icon32.png icon16.png

# Create the disk image
rm -rf $EXE $EXE.dmg
mkdir $EXE
cp ../README.md ./$EXE/README.txt
cp -r ../gui/examples ./$EXE/examples
mv $APP ./$EXE
mkdir $EXE/.Trash
hdiutil create $EXE.dmg -volname "$EXE $VERSION" -srcfolder $EXE
rm -rf $EXE
mv $EXE.dmg ..
