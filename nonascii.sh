#!/bin/bash


perl -ne 'print "$. $_" if m/[\x80-\xFF]/' $1
