#!/usr/bin/env escript

-module(install).
-export([main/0, main/1]).

-define(BACKUP_SUFFIX, ".backup-pre-shell-history").
-define(BACKUP, "group.beam" ++ ?BACKUP_SUFFIX).

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

    update_kernel_app_file(Path ++ "/kernel.app"),
    update_modules(Path, Version).


%% Inject "group_history" into the modules list of kernel.app
%% Otherwise creating releases from this install results in errors
%% due to group trying to hit up a missing group_history module.
update_kernel_app_file(Path) ->
    {ok, [{application, kernel, Sections}]} = file:consult(Path),
    ModList = proplists:get_value(modules, Sections),
    case lists:member(group_history, ModList) of
        true -> 
            io:format("group_history already in modules list of kernel.app~n"),
            ok;
        false ->
            NewModList = [group_history | ModList],
            NewSections = [ {modules, NewModList} 
                            | proplists:delete(modules, Sections) ],
            AppSpec = {application, kernel, NewSections},
            AppContents = io_lib:format("~p.",[AppSpec]),
            BackupPath = Path ++ ?BACKUP_SUFFIX,
            file:delete(BackupPath),
            {ok, _} = file:copy(Path, BackupPath),
            file:delete(Path),
            ok = file:write_file(Path, AppContents),
            io:format("Injected 'group_history' into modules list of kernel.app~n"),
            io:format("Backup of kernel.app saved to: ~s~n", [BackupPath]),
            ok
    end.

update_modules(Path, Version) ->
    %% check if the backup exists and only create one if it doesn't
    Backup = lists:flatten(io_lib:format("~s/" ++ ?BACKUP, [Path])),
    BackupExists = filelib:is_regular(Backup),
    case BackupExists of
        true  ->
            io:format("Backup of group.beam exists~n"),
            ok;
        false ->
            BackupCmd = io_lib:format(
                  "cp -n \"~s/group.beam\" \"~s/" ++ ?BACKUP ++ "\"",
                  [Path, Path]
                 ),
            io:format("Backing up existing file~n"),
            os:cmd(BackupCmd)
    end,
    %% add in the modified group.erl and the new group_history.erl
    CopyCmd = io_lib:format(
            "cp ebin/~s/*.beam \"~s\"",
            [Version, Path]
            ),
    io:format("Installing...~n"),
    os:cmd(CopyCmd).
