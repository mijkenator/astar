timer:tc(astar_test, test, [{1,1},{100,100}]).

fprof:apply(astar_test, test, [{1,1},{100,100}]).
fprof:profile().
fprof:analyse().


----------------------------------------------------------------
I can recommend you this tool: https://github.com/virtan/eep

You will get something like this https://raw.github.com/virtan/eep/master/doc/sshot1.png as a result.

Step by step instruction for profiling all processes on running system:

On target system:

1> eep:start_file_tracing("file_name"), timer:sleep(20000), eep:stop_tracing().
$ scp -C $PWD/file_name.trace desktop:
On desktop:

1> eep:convert_tracing("file_name").
$ kcachegrind callgrind.out.file_name
------------------------------------------------------------------

eprof:start().
eprof:start_profiling([self()]).
astar_test:test({1,1},{990,990}).
eprof:stop_profiling().
eprof:analyze(total).

