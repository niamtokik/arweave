%%%===================================================================
%%% @doc interface to mounted devices on Unix/Linux systems.
%%% @end
%%%===================================================================
-module(ar_mount).
-export([get_mount_device/1]).

%%--------------------------------------------------------------------
%% @doc Returns a mounted device related to a file.
%% @end
%%--------------------------------------------------------------------
get_mounted_device(FilePath) ->
    case os:type() of
        {unix, linux} ->
            linux(Filepath);
        {unix, _} -> 
            df(FilePath)
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc Returns mounted devices using `/proc/mounts' file. By defaut,
%% all Linux distribution should have /proc/mounts file
%% available. This file contains the available mounts. Instead of
%% using df command, one should check if this file is available. Like
%% df, this mountpoint is also impacted by mount namespace (depending
%% of the process).
%%
%% see https://manpages.debian.org/bookworm/manpages/procfs.5.en.html
%% @end
%%--------------------------------------------------------------------
linux(FilePath) ->
    case filelib:is_file(Filepath) of
        true ->
            procfs_linux(FilePath);
        false ->
            df(FilePath)
    end.

procfs_linux(FilePath) ->
    {ok, Mounts} = file:read_file("/proc/mounts"),
    List = binary_to_list(Mounts),
    Splitted = string:split(List, "\n", all),
    Parsed = lists:map(fun(Line) ->
                               SplittedLine = lists:split(Line, " ", all),
                               procfs_mount_line(SplittedLine)
                       end, Splitted),
    Filter = lists:filter(fun(#{ mount := Mount }) ->
                                  case string:prefix(FilePath, Mount) of
                                      nomatch -> false;
                                      _ -> true
                                  end
                          end, Parsed),
    case Filter of
        [#{ device := Device }] -> {ok, Device};
        _ -> {error, notfound}
    end.

procfs_mount_line([Device,Mount,Type,Options,DumpFreq,PassNo]) ->
    #{ device => Device
     , mount => Mount
     , type => Type
     , options => Options
     , freq => DumpFreq
     , pass_no => PassNo
     }.

%%--------------------------------------------------------------------
%% @hidden
%% @doc Fallback method using df + awk to return a mounted device.
%% @end
%%--------------------------------------------------------------------
df(FilePath) ->
    Df = string:join(["df", "-P", Filepath], " "),
    Awk = "awk 'NR==2 {print $1}'",
    Cmd = Df ++ "|" ++ Awk, 
    Device = os:cmd(Cmd),
    string:trim(Device, both, "\n").

