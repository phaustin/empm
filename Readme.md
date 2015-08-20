#Source code for Su et al., 1998:  Linear eddy modeling ...

## Citation

    @ARTICLE{Su98,
    author = {Su, Chwen-Wei and Krueger, Steven K. and McMurtry, Patrick A. and Austin, Philip H.},"
    title = {Linear eddy modeling of droplet spectral evolution during entrainment and mixing in cumulus clouds},"
    journal = {Atmospheric Research},"
    volume = {47-48},
    year = {98},
    pages = {41-58},
    doi = {10.1016/S0169-8095(98)00039-8},
    resource = {http://linkinghub.elsevier.com/retrieve/pii/S0169809598000398},
    alternate_resource = {http://clouds.eos.ubc.ca/~phil/austinpapers/su98.pdf}
    }

## License

https://creativecommons.org/licenses/by/3.0/


## build instructions

### tested on osx maverick with homebrew gfortran 4.9

These files originally downloaded from

http://www.inscc.utah.edu/~krueger/EMPM/c_eps29_small.tar.gz


1. fork and clone this repository to directory empm

1. cd empm/src

1. compile the code:

   $ bash -v command1

   which compiles the executable c_eps29x with some warnings

1. Run this in src.:

        $ ./c_eps29x > c_eps29x.output &

    This produces the following files in the local src directory:

        -rw-r--r--    1 phil  staff    323000 Aug 20 15:42 ave.dat
        -rw-r--r--    1 phil  staff     30966 Aug 20 15:42 c_eps29x.output
        -rw-r--r--    1 phil  staff       200 Aug 20 15:42 entm.dat
        -rw-r--r--    1 phil  staff      4480 Aug 20 15:42 findex.dat
        -rw-r--r--    1 phil  staff        25 Aug 20 15:42 fort.10
        -rw-r--r--    1 phil  staff        25 Aug 20 15:42 fort.50
        -rw-r--r--    1 phil  staff     52000 Aug 20 15:42 fort.75
        -rw-r--r--    1 phil  staff     45000 Aug 20 15:42 fort.77
        -rw-r--r--    1 phil  staff        25 Aug 20 15:42 fort.9
        -rw-r--r--    1 phil  staff    103020 Aug 20 15:42 fort.94
        -rw-r--r--    1 phil  staff  48068132 Aug 20 15:42 qvtime.dat
        -rw-r--r--    1 phil  staff    252000 Aug 20 15:42 qwmov.dat
        -rw-r--r--    1 phil  staff    152000 Aug 20 15:42 r2_mean_time.dat
        -rw-r--r--    1 phil  staff    152000 Aug 20 15:42 r3_mean_time.dat
        -rw-r--r--    1 phil  staff    152000 Aug 20 15:42 r_mean_time.dat
        -rw-r--r--    1 phil  staff    152000 Aug 20 15:42 rmov.dat
        -rw-r--r--    1 phil  staff  16020000 Aug 20 15:42 rtime.dat
        -rw-r--r--    1 phil  staff    816816 Aug 20 15:42 sca2p1.dat
        -rw-r--r--    1 phil  staff    816816 Aug 20 15:42 sca2p2.dat
        -rw-r--r--    1 phil  staff      3312 Aug 20 15:42 scree.dat
        -rw-r--r--    1 phil  staff     46000 Aug 20 15:42 super.dat
        -rw-r--r--    1 phil  staff    816816 Aug 20 15:42 sus.pdf
        -rw-r--r--    1 phil  staff     13000 Aug 20 15:42 t_index.dat
        -rw-r--r--    1 phil  staff  48068132 Aug 20 15:42 temptime.dat
        -rw-r--r--    1 phil  staff  32020000 Aug 20 15:42 xtime.dat
        -rw-r--r--    1 phil  staff         0 Aug 20 15:39 dgm.dat
        -rw-r--r--    1 phil  staff      3185 Aug 20 15:39 fort.76

1. compile and run the reformatting routine for temperature and vapor

        $ gfortran -o read_writeTQ read_writeTQ.f

   which reads ./qvtime.dat and writes ../output/qvtime_easy.dat

1. compile and run the

        $ gfortran -o empm_read_new_incus empm_read_new_incus.f

   which reads ./rtime.dat, ./xtime.dat and ./t_index.dat and writes

   ../output/mean_vol_radius.dat and ../output/mean_location.dat


