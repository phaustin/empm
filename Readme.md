Initial check-in Files downloaded from  http://www.inscc.utah.edu/~krueger/EMPM/

Creation of tar files sent to Dave Turner, 5-11-07:

1. mkdir test0
   cd test0

1. tar -xvf c_eps29_small.tar

1. cp test2/command1 .

1. csh command1  [creates c_eps29x]

1. c_eps29x > c_eps29x.output &

1. f77 read_writeTQ.f => qvtime_easy.dat

1. f77 empm_read_new_incus.f => mean_vol_radius.dat, mean_location.dat

1. ls -lt > file_list.txt

1. delete large output files:
-rw-r--r--  1 skrueger meteorology 48084164 May 11 16:52 qvtime.dat
-rw-r--r--  1 skrueger meteorology 48084164 May 11 16:52 temptime.dat
-rw-r--r--  1 skrueger meteorology 48016000 May 11 17:22 qvtime_easy.dat
-rw-r--r--  1 skrueger meteorology 32036000 May 11 16:52 xtime.dat
-rw-r--r--  1 skrueger meteorology 16036000 May 11 16:52 rtime.dat

1. delete some unneeded files: 
indextime.dat qwmov.dat fort.11

1. make tar files: 
/data/skrueger/skrueger/EMPM/empm.tar.gz
/data/skrueger/skrueger/EMPM/output.tar.gz
