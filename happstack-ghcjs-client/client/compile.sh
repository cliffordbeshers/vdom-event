#!/bin/bash

function cabalcmd () { runhaskell Setup $* ; }

function compile () {
    case $1 in
	configure)
	    ( cd .. ; cabalcmd configure --ghcjs ) ;;
	build)
	    ( cd .. ; cabalcmd build ) ;;
	clean)
	    ( cd .. ; cabalcmd clean ) ;;
	all)
	    ( compile clean ; compile configure ; compile build ) ;;
	*) 
	    echo "unknown command: $1" 2>&1 ; exit 1 ;;
    esac
}

compile $*
