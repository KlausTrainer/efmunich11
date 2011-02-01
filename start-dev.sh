#!/bin/bash

usage ()
{
	echo "usage: $0 [-i|--interactive]"
	exit -1
}


cd `dirname $0`

for (( i=1; i<$#; i++ ))
do
	case ${!i} in
	-i)
		INTERACTIVE=1
		;;
	--interactive)
		INTERACTIVE=1
		;;
	*)
		echo "Unknown option: ${!i}"
		usage
		;;
	esac
done

if [ -z $INTERACTIVE ]; then
	DETACHED="-detached"
fi

exec ERL_LIBS=.. erl $DETACHED -pa $PWD/ebin -sname term_cache -s term_cache -s reloader
