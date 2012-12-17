#!/usr/bin/env escript

-module(install).
-export([main/0, main/1]).

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

    BackupCmd = io_lib:format(
      "cp -n \"~s/group.beam\" \"~s/group.beam.backup-pre-shell-history\"",
      [Path, Path]
    ),
    io:format("Backing up existing file~n"),
    os:cmd(BackupCmd),

    CopyCmd = io_lib:format(
      "cp ebin/~s/*.beam \"~s\"",
      [Version, Path]
    ),
    io:format("Installing...~n"),
    os:cmd(CopyCmd).
