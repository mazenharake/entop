%% Copyright (c) 2010, Mazen Harake
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%     * Redistributions of source code must retain the above copyright notice,
%%       this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(entop_view).
-include("entop.hrl").
-include_lib("cecho/include/cecho.hrl").

%% Module API
-export([start/1, reload/1]).

%% Defines
-define(MAX_HLINE, 300).

%% =============================================================================
%% Module API
%% =============================================================================
start(State) ->
    Parent = self(),
    NState = load_remote_static_data(State),
    remote_load_code(NState#state.remote_module, State#state.node),
    ViewPid = erlang:spawn(fun() -> init(Parent, NState) end),
    receive continue -> ok end,
    ViewPid.

reload(ViewPid) ->
    ViewPid ! reload.

%% =============================================================================
%% Internal Functions
%% =============================================================================
load_remote_static_data(State) ->
    RPC = fun(M, F, A) -> rpc:call(State#state.node, M, F, A) end,
    Otp = RPC(erlang, system_info, [otp_release]),
    Erts = RPC(erlang, system_info, [version]),
    {Os1, Os2} = RPC(os, type, []),
    OsVers = RPC(os, version, []),
    Flags = [{cpus, RPC(erlang, system_info, [logical_processors])},
	     {smp, RPC(erlang, system_info, [smp_support])},
	     {a_threads, RPC(erlang, system_info, [thread_pool_size])},
	     {kpoll, RPC(erlang, system_info, [kernel_poll])}],
    State#state{ otp_version = Otp, erts_version = Erts,
		 os_fam = Os1, os = Os2, os_version = OsVers, node_flags = Flags }.

remote_load_code(Module, Node) ->
    {_, Binary, Filename} = code:get_object_code(Module),
    rpc:call(Node, code, load_binary, [Module, Filename, Binary]).

init(Parent, State) ->
    process_flag(trap_exit, true),
    application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    ok = cecho:keypad(?ceSTDSCR, true),
    NState = init_callback(State),
    print_nodeinfo(NState),
    Parent ! continue,
    self() ! time_update,
    loop(Parent, NState).

init_callback(State) ->
    case (State#state.callback):init(State#state.node) of
	{ok, {Columns, DefaultSort}, CBState} when DefaultSort =< length(Columns) 
						   andalso DefaultSort >= 1 ->
	    NSort = DefaultSort;
	{ok, {Columns, _}, CBState} ->
	    NSort = 1
    end,
    State#state{ columns = Columns, cbstate = CBState, sort = NSort }.

print_nodeinfo(State) ->
    cecho:move(0, 0),
    cecho:hline($ , ?MAX_HLINE),
    {Mj, Md, Mi} = State#state.os_version,
    OsVers = lists:concat([Mj,".",Md,".",Mi]),
    Head = io_lib:format("Node: ~p (~s/~s) ~p (~p ~s)~s", 
			 [State#state.node, State#state.otp_version, 
			  State#state.erts_version, State#state.os_fam,
			  State#state.os, OsVers, flags2str(State#state.node_flags)]),
    ok = cecho:mvaddstr(0,0,lists:flatten(Head)).

flags2str([]) -> [];
flags2str([{cpus, N}|Rest]) -> 
    [" CPU:"++integer_to_list(N)|flags2str(Rest)];
flags2str([{smp, true}|Rest]) ->
    [" SMP"|flags2str(Rest)];
flags2str([{a_threads, N}|Rest]) ->
    [" +A:"++integer_to_list(N)|flags2str(Rest)];
flags2str([{kpoll, true}|Rest]) ->
    [" +K"|flags2str(Rest)];
flags2str([_|Rest]) ->
    flags2str(Rest).

loop(Parent, State) ->
    receive
	time_update ->
	    State2 = update_screen(State),
	    erlang:send_after(State2#state.interval, self(), time_update),
	    loop(Parent, State2);
	force_update ->
	    State2 = update_screen(State),
	    loop(Parent, State2);
	{sort, N} when is_integer(N) ->
	    State2 = update_sort_screen(State, N),
	    loop(Parent, State2);
	{sort, Direction} ->
	    case Direction of
		next -> State2 = update_sort_screen(State, State#state.sort + 1);
		prev -> State2 = update_sort_screen(State, State#state.sort - 1)
	    end,
	    loop(Parent, State2);
	reverse_sort ->
	    State2 = update_screen(State#state{ reverse_sort = (not State#state.reverse_sort) }),
	    loop(Parent, State2);
	{'EXIT', Parent, _} ->
	    ok
    end.

update_sort_screen(State, N) ->
    if N >= 1 andalso N =< length(State#state.columns) ->
	    update_screen(State#state{ sort = N });
       true -> State
    end.

update_screen(State) ->
    print_nodeinfo(State),
    draw_title_bar(State),
    {Time, {ok, SysInfo, PInfo}} = timer:tc(rpc, call, [State#state.node, State#state.remote_module, get_data, []]),
    print_showinfo(State, Time),
    {Headers, State1} = handle_system_info(SysInfo, State),
    lists:foldl(fun(Header, Y) -> cecho:mvaddstr(Y, 0, Header), Y + 1 end, 1, Headers),
    {ProcList, State2} = handle_process_info(PInfo, State1), 
    SortedProcList = sort(ProcList, State),
    {Y, _} = cecho:getmaxyx(),
    StartY = (Y-(Y-7)),
    lists:foreach(fun(N) -> cecho:move(N, 0), cecho:hline($ , ?MAX_HLINE) end, lists:seq(StartY, Y)),
    update_rows(SortedProcList, State2#state.columns, StartY, Y),
    cecho:refresh(),
    State2.

draw_title_bar(State) ->
    cecho:move(6, 0),
    cecho:attron(?ceA_REVERSE),
    cecho:hline($ , ?MAX_HLINE),
    draw_title_bar(State#state.columns, 0),
    cecho:attroff(?ceA_REVERSE).

draw_title_bar([], _) -> ok;
draw_title_bar([{Title, Width, Options}|Rest], Offset) ->
    Align = proplists:get_value(align, Options, left),
    cecho:mvaddstr(6, Offset, string:Align(Title, Width)++" "),
    draw_title_bar(Rest, Offset + Width + 1).

print_showinfo(State, RoundTripTime) ->
    cecho:move(5, 0),
    cecho:hline($ , ?MAX_HLINE),
    ColName = element(1,lists:nth(State#state.sort, State#state.columns)),
    SortName = if State#state.reverse_sort -> "Descending"; true -> "Ascending" end,
    Showing = io_lib:format("Showing: Interval ~p ms, Sorting on ~p (~s), Retrieved in ~p ms", 
			    [State#state.interval, ColName, SortName, RoundTripTime div 1000]),
    cecho:mvaddstr(5,0, lists:flatten(Showing)).

handle_system_info(SystemInfo, State) ->
    {ok, Headers, NCBState} = (State#state.callback):header(SystemInfo, State#state.cbstate),
    {Headers, State#state{ cbstate = NCBState }}.

handle_process_info(ProcessInfoList, State) ->
    hpi(ProcessInfoList, State, []).

hpi([], State, Acc) ->
    {Acc, State};
hpi([ProcessInfo|Rest], State, Acc) ->
    {ok, Row, NCBState} =
	(State#state.callback):row(ProcessInfo, State#state.cbstate),
    hpi(Rest, State#state{ cbstate = NCBState }, [Row|Acc]).

sort(ProcList, State) ->
    Sorted = lists:keysort(State#state.sort, ProcList),
    case State#state.reverse_sort of
	true ->
	    lists:reverse(Sorted);
	false ->
	    Sorted
    end.

update_rows(ProcValuesList, _, LineNumber, Max) when LineNumber == Max orelse ProcValuesList == [] -> ok;
update_rows([RowValues|Rest], Columns, LineNumber, Max) ->
    update_row(tuple_to_list(RowValues), Columns, LineNumber, 0),
    update_rows(Rest, Columns, LineNumber + 1, Max).

update_row(R, C, _, _) when R == [] orelse C == [] -> ok;
update_row([RowColValue|Rest], [{_,Width,Options}|RestColumns], LineNumber, Offset) ->
    StrColVal = if is_list(RowColValue) ->
			RowColValue;
		   true ->
			lists:flatten(io_lib:format("~1000p",[RowColValue]))
		end,
    Aligned = case proplists:get_value(align, Options) of
		  right ->
		      string:right(StrColVal, Width);
		  _ ->
		      string:left(StrColVal, Width)
	      end,
    cecho:mvaddstr(LineNumber, Offset, Aligned),
    update_row(Rest, RestColumns, LineNumber, Offset+Width+1).
    
    




