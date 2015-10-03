#!/bin/bash

set -u

work_dir="`pwd`/$1"

if [ ! -d ${work_dir} ]; then
    mkdir ${work_dir}
fi

base_url=http://spamassassin.apache.org/publiccorpus/
date=20021010
ham=$2
spam=$3

for target in ${ham} ${spam} ; do
    if [ ! -d ${work_dir}/${target} ]; then
        wget ${base_url}/${date}_${target}.tar.bz2 -P ${work_dir}
        tar jxvf ${work_dir}/${date}_${target}.tar.bz2 -C ${work_dir}
        
        cd ${work_dir}/${target}
        for file in `ls` ; do
            nkf --overwrite -w ${file}
            # For unexplained reasons, some files require
            # several nkf commands to convert to UTF or ASCII.
            # We remove such files
            nkf -g ${file} | egrep 'UTF|ASCII' >& /dev/null
            if [ $? -ne 0 ]; then
                rm ${file}
            fi
        done
        cd ../..
    fi
done
