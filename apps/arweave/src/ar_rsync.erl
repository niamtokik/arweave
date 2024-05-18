%%%===================================================================
%%% @TODO add rsync as dependencies
%%% @TODO find an alternative to rsync if needed
%%%===================================================================
-module(ar_rsync).
-export([exec/2]).

execute(Filepath, TmpFilepath) ->
    DefragCmd = io_lib:format("rsync --sparse --quiet ~ts ~ts", [Filepath, TmpFilepath]),
    %% We expect nothing to be returned on successful calls.
    [] = os:cmd(DefragCmd).

