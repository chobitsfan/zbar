#!/bin/bash

unset ERR

DIR="/home/pi/zbar-0.23.90"
ZBARIMG="/home/pi/zbar-0.23.90/zbarimg/zbarimg --nodbus"

test()
{
	if [ "$2" != "" ]; then
	        i="$DIR/examples/$2"
		j="$1 $2"
	else
	        i="$DIR/examples/$1"
		j="$1"
	fi;
	if [ "$2" != "" ]; then
		CMD="$ZBARIMG $1"
	else
		CMD="$ZBARIMG"
	fi
	CK=`$CMD "$i" 2>/dev/null|sha1sum|cut -d" " -f1`
	ORG=`grep "zbarimg $j" "$DIR/examples/sha1sum"|cut -d " " -f1`

	if [ "$CK" != "$ORG" ]; then
		echo "FAILED: $i ($CK instead of $ORG)"
		echo -e "\tcmd: $CMD '$i'"
		echo -en "\tresults: "
		$CMD "$i" 2>/dev/null
		ERR=1
	fi
}

if [ "1" == "1" ]; then
	test code-128.png
fi

if [ "1" == "1" ]; then
        test code-93.png
fi

if [ "1" == "1" ]; then
        test code-39.png
fi

if [ "1" == "1" ]; then
        test codabar.png
fi

if [ "1" == "1" ]; then
        test databar.png
        test databar-exp.png
fi

if [ "1" == "1" ]; then
        test -Sean2.enable ean-2.png
        test -Sean5.enable ean-5.png
        test ean-8.png
        test ean-13.png
        test -Sisbn10.enable ean-13.png
        test -Sisbn13.enable ean-13.png
        test -Supca.enable code-upc-a.png
fi

if [ "1" == "1" ]; then
        test i2-5.png
fi

if [ "1" == "1" ]; then
        test qr-code.png
        test -Stest-inverted qr-code-inverted.png
        test '--raw --oneshot -Sbinary' qr-code-binary.png
fi

if [ "1" == "1" ]; then
        test sqcode1-generated.png
        test sqcode1-scanned.png
fi

# The pdf417 code is incomplete: it doesn't output any results
#
#if [ "0" == "1" ]; then
#        test code-pdf417.png
#fi

if [ "$ERR" == "" ]; then
	echo "zbarimg PASSED."
else
	exit 1
fi

