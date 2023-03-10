#!/bin/bash

DIR="/home/pi/zbar-0.23.90"
BDIR="/home/pi/zbar-0.23.90"
LOG="/tmp/zbar_dbus_test_$$.log"
LOG_BIN="/tmp/zbar_dbus_test_$$.bin"

EXPECTED="7294377b69fb00c7e0811429ab7a42cc8cecfda0"
EXPECTED_BIN="df896e459e47a7d392031a7d4962722a143e276b"


$BDIR/test/test_dbus -c2 -t5 --log=$LOG --bin-log=$LOG_BIN &
PID=$!

trap "rm -r $LOG $LOG_BIN" EXIT

$BDIR/zbarimg/zbarimg $DIR/examples/code-128.png 2>/dev/null >/dev/null
$BDIR/zbarimg/zbarimg -Sbinary $DIR/examples/qr-code-binary.png 2>/dev/null >/dev/null

wait $PID

if [ ! -s $LOG ] || [ ! -s $LOG_BIN ]; then
	echo "FAILED: nothing received via D-Bus"
    exit -2
fi

CK="`cat $LOG  |sha1sum |cut -d" " -f 1`"
if [ "x$CK" != "x$EXPECTED" ]; then
    echo "FAILED: $CK instead of $EXPECTED"
    exit -2
fi

CK_BIN="`cat $LOG_BIN  |sha1sum |cut -d" " -f 1`"
if [ "x$CK_BIN" != "x$EXPECTED_BIN" ]; then
    echo "FAILED: $CK_BIN instead of $EXPECTED_BIN"
    exit -2
fi

echo "D-Bus PASSED."
