#!/bin/sh

export IN=in.pas
export OUT=out.pas

exec tpl -q --ns pascal-formatter.pro
