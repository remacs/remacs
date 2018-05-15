#!/bin/bash

echo "Testing build and compile of remacs with arguments: $@"

rust_src/admin/build-and-run-remacs.sh "$@"

result=$?

if [ $? -ne 0 ]; then
    exit $result
fi

read -r -p "Did it work? [Ny]" work
case "$work" in
    Y|y)
        exit 0
        ;;
    *)
        exit 1
        ;;
esac
