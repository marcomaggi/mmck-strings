# gdbinit --
#
# Copy this  file in  the directory  from which we  run the  test programs,  which is
# usually  "$(top_builddir)/tests".  Run  gdb as  "gdb --args"  so that  command line
# arguments in the Makefile are handed to the program rather than to "gdb".
#
# From the commmand line of make:
#
#   $ (CHICKEN_REPOSITORY_PATH=$PWD/lib:$PWD/tests:$CHICKEN_REPOSITORY_PATH \
#        gdb --init-command=../gdbinit --args tests/test-demo.exe)
#

directory ./lib ./tests

### end of file
