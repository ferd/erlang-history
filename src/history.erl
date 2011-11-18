-module(history).
-export([load/0, add/1]).

-define(DEFAULT_HIST_FILE, ".erlang-hist").
-define(DEFAULT_HIST_SIZE, 500).
-define(TABLE, shell_group_hist).
-define(DEFAULT_AUTOSAVE, 500).
-define(DEFAULT_DROP, []).

%% History is node-based, but history of many jobs on a single node
%% are mixed in together.
-record(opts, {hist=true, hist_file, hist_size}).

%%% PUBLIC
%% Loads the shell history from memory. This function should only be
%% called from group:server/3 to inject itself in the previous commands
%% stack.
load() ->
    case opts() of
        #opts{hist=false} ->
            [];
        #opts{hist_file=F, hist_size=S} ->
            wait_for_kernel_safe_sup(),
            %% We cannot repair the table automatically. The current process
            %% is handling output and dets repairing a table outputs a message.
            %% Due to how the IO protocol works, this leads to a deadlock where
            %% we wait to reply to ourselves in some circumstances.
            case dets:open_file(?TABLE, [{file,F}, {auto_save, opt(hist_auto_save)}, {repair, false}]) of
                {ok, ?TABLE} -> load_history(S);
                {error, {needs_repair, F}} -> repair_table(F)
            end
    end.

load_history(S) ->
    Ct = case dets:lookup(?TABLE, ct) of
        [] -> % 1st run
            dets:insert(?TABLE, {ct,1}),
            1;
        [{ct,OldCt}] -> OldCt
    end,
    load(Ct, Ct-S).

%% Repairing the table means we need to spawn a new process to do it for us.
%% That process will then try to message the current group leader with mentions
%% of trying to repair the table. We need to absorb these, let the process
%% repair, then close the table in order for us to open it again and finally
%% be free!
repair_table(F) ->
    %% Start a process to open and close the table to repair it, different
    %% from the current shell process.
    R = make_ref(),
    S = self(),
    spawn(fun() ->
        {ok, ?TABLE} = dets:open_file(?TABLE, [{file,F},{repair,force}]),
        dets:close(?TABLE),
        S ! R
    end),
    %% Messages from the IO protocol will only need to be received if we
    %% currently are the group leader (in this case, the 'user' process).
    %% if this is not 'user', we can move on.
    case process_info(self(), registered_name) of
        {registered_name, user} ->
            receive
                {io_request,From,ReplyAs,
                {put_chars,_Encoding,io_lib,format,
                ["dets:"++_, [F, [_|_]]]}} -> From ! {io_reply, ReplyAs, ok}
            end;
        _ -> ok
    end,
    %% now we wait for the worker to close the table, telling us it's safe
    %% to load it on our own.
    receive
        R -> ok
    end,
    load().

%% Adds a given line to the history,
add(Line) -> add(Line, opt(hist)).

%% only add lines that do not match something to drop from history. The behaviour
%% to avoid storing empty or duplicate lines is in fact implemented within
%% group:save_line_buffer/2.
add(Line, true) ->
    case lists:member(Line, opt(hist_drop)) of
        false ->
            [{ct, Ct}] = dets:lookup(?TABLE, ct),
            dets:insert(?TABLE, {Ct, Line}),
            dets:delete(?TABLE, Ct-opt(hist_size)),
            dets:update_counter(?TABLE, ct, {2,1});
        true ->
            ok
    end;
add(_, false) -> ok.

%%% PRIVATE
%% Gets a short record of vital options when setting things up.
opts() ->
    #opts{hist=opt(hist),
          hist_file=opt(hist_file),
          hist_size=opt(hist_size)}.

%% Defines whether history should be allowed at all.
opt(hist) ->
    case application:get_env(kernel, hist) of
        {ok, true} -> true;
        {ok, false} -> false;
        _Default -> true
    end;
%% Defines what the base name and path of the history file will be.
%% By default, the file sits in the user's home directory as
%% '.erlang-history.'. All filenames get the node name apended
%% to them.
opt(hist_file) ->
    case {opt(hist), application:get_env(kernel, hist_file)} of
        {true, undefined} ->
            case init:get_argument(home) of
                {ok, [[Home]]} ->
                    Name = filename:join([Home, ?DEFAULT_HIST_FILE]),
                    application:set_env(kernel, hist_file, Name),
                    opt(hist_file);
                _ ->
                    error_logger:error_msg("No place found to save shell history"),
                    erlang:error(badarg)
            end;
        {true, {ok, Val}} -> Val++"."++atom_to_list(node());
        {false, _} -> undefined
    end;
%% Defines how many commands should be kept in memory. Default is 500.
opt(hist_size) ->
    case {opt(hist), application:get_env(kernel, hist_size)} of
        {true, undefined} ->
            application:set_env(kernel, hist_size, ?DEFAULT_HIST_SIZE),
            ?DEFAULT_HIST_SIZE;
        {true, {ok,Val}} -> Val;
        {false, _} -> undefined
    end;
%% This handles the delay of auto-saving of DETS. This isn't public
%% and the value is currently very short so that shell crashes do not
%% corrupt the file history.
opt(hist_auto_save) ->
    case application:get_env(kernel, hist_auto_save) of
        undefined ->
            application:set_env(kernel, hist_auto_save, ?DEFAULT_AUTOSAVE),
            ?DEFAULT_AUTOSAVE;
        {ok, V} -> V
    end;
%% Allows to define a list of strings that should not be kept in history.
%% one example would be ["q().","init:stop().","halt()."] if you do not
%% want to keep ways to shut down the shell.
opt(hist_drop) ->
    case application:get_env(kernel, hist_drop) of
        undefined ->
            application:set_env(kernel, hist_drop, ?DEFAULT_DROP),
            ?DEFAULT_DROP;
        {ok, V} when is_list(V) -> [Ln++"\n" || Ln <- V];
        {ok, _} -> ?DEFAULT_DROP
    end.

%% Because loading the shell happens really damn early, processes we depend on
%% might not be there yet. Luckily, the load function is called from the shell
%% after a new process has been spawned, so we can block in here
wait_for_kernel_safe_sup() ->
    case whereis(kernel_safe_sup) of
        undefined ->
            timer:sleep(50),
            wait_for_kernel_safe_sup();
        _ -> ok
    end.

%% Load all the elements previously saved in history
load(N, _) when N =< 0 -> [];
load(N, M) when N =< M -> truncate(M), [];
load(N, M) ->
    case dets:lookup(?TABLE, N-1) of
        [] -> []; % nothing in history
        [{_,Entry}] -> [Entry | load(N-1,M)]
    end.

%% If the history size was changed between two shell sessions, we have to
%% truncate the old history.
truncate(N) when N =< 0 -> ok;
truncate(N) ->
    dets:delete(?TABLE, N),
    truncate(N-1).
