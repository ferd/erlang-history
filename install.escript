#!/usr/bin/env escript

-module(install).
-export([main/0, main/1]).

-define(BACKUP, "group.beam.backup-pre-shell-history").

main(_) -> main().

main() ->
    Path = io_lib:format("~s/ebin",[code:lib_dir(kernel)]),
    Version = io_lib:format(
        "~s",
        [ element(
            3,
            lists:keyfind(kernel,1,application:which_applications())
          )
        ]
    ),
    io:format("Path = ~s~nVersion = ~s~n", [Path, Version]),

    % check if the backup exists and only continue if it doesn't
    Backup = lists:flatten(io_lib:format("~s/" ++ ?BACKUP, [Path])),
    BackupExists = filelib:is_regular(Backup),
    case BackupExists of
        true  ->
            ok;
        false ->
            BackupCmd = io_lib:format(
                  "cp -n \"~s/group.beam\" \"~s/" ++ ?BACKUP ++ "\"",
                  [Path, Path]
                 ),
            io:format("Backing up existing file~n"),
            os:cmd(BackupCmd),

            CopyCmd = io_lib:format(
                "cp ebin/~s/*.beam \"~s\"",
                [Version, Path]
               ),
            io:format("Installing...~n"),
            os:cmd(CopyCmd)
    end.
