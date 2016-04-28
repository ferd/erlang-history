#!/usr/bin/env escript

-module(install).
-export([main/0, main/1]).

-define(BACKUP_SUFFIX, ".backup-pre-shell-history").
-define(BACKUP, "group.beam" ++ ?BACKUP_SUFFIX).


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
    main([Path, Version]).

main([Path, Version]) ->
    io:format("Path = ~s~nVersion = ~s~n", [Path, Version]),
    update_kernel_app_file(Path ++ "/kernel.app"),
    update_modules(Path, Version);
main(_) -> main().


copy_files(Wildcard, Destination, Modes) ->
    N = lists:foldl(
        fun(Source, Acc) ->
            Target = case filelib:is_dir(Destination) of
                true  -> filename:join(Destination, filename:basename(Source));
                false -> Destination
            end,
            io:format("~ts~n", [Source]),
            Acc + case file:copy(Source, {Target, Modes}) of
                {ok, _BytesCopied} ->
                    {ok, FileInfo} = file:read_file_info(Source),
                    ok = file:write_file_info(Target, FileInfo),
                    1;
                {error, Reason} ->
                    io:format("~ts~n", [file:format_error(Reason)]),
                    0
            end
        end,
        0,
        filelib:wildcard(Wildcard)),
    io:format("~p file(s) copied~n", [N]).


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
            copy_files(Path, BackupPath, []),
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
            io:format("Backing up existing file~n"),
            copy_files(
                filename:join(Path, "group.beam"),
                filename:join(Path, ?BACKUP), [exclusive])
    end,
    %% add in the modified group.erl and the new group_history.erl
    io:format("Installing...~n"),
    copy_files(filename:join(["ebin", Version, "*.beam"]), Path, []).
