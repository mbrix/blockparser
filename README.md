blockparser
===========

An Erlang Bitcoin disk block parser

This is an example parser for the on-disk Bitcoin blockchain, as well as an experiment in concurrent programming. 

You can read more about this particular experiment on my blog,

http://www.brantonbits.com/blog/2014/05/29/parsing-the-blockchain-with-erlang/

To run this code:
Grab Erlang R17+, watch out for old distro releases, I compiled from source.
Grab Rebar, the defacto Erlang build tool from Basho -> https://github.com/basho/rebar.git
rebar get-deps
cd deps/poolboy
make
cd ..
make all
cp ~/.bitcoin/blocks/blk00000.dat .
make shell
application:start(poolboy).
application:start(blockparser).
blockparser:parse("blk00000.dat").

This will synchronously parse the block and dump a bunch of raw data into blk00000.dat.csv

parse_dir_seq() will sequentially process every file in the directory
parse_dir() will parse them in parallel using a bastardized work splitting technique.
