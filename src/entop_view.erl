%%==============================================================================
%% Copyright (c) 2017, Mazen Harake
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
%%==============================================================================

-module(entop_view).

-include_lib("cecho/include/cecho.hrl").

%% Module API
-export([start/1, reload/1]).

%% Defines
-define(MAX_HLINE, 300).

%% Records
-record(state, { callback = entop_format,
                 remote_module = entop_collector,
                 maxyx,
                 columns,
                 cbstate,
                 node,
                 otp_version,
                 erts_version,
                 os_fam,
                 os,
                 os_version,
                 node_flags,
                 interval = 2000,
                 reverse_sort = true,
                 sort = 0,
                 connected = false,
                 last_reductions = dict:new() }).

%% =============================================================================
%% Module API
%% =============================================================================
start(Node) ->
  Parent = self(),
  case net_kernel:connect_node(Node) of
    true ->
      State = #state { node = Node, connected = true },
      NState = load_remote_static_data(State),
      remote_load_code(NState#state.remote_module, State#state.node),
      ViewPid = erlang:spawn(fun() -> init(Parent, NState) end),
      receive continue -> ok end,
      {ok, ViewPid};
    false ->
      {error, cant_connect}
  end.

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
               os_fam = Os1, os = Os2, os_version = OsVers,
               node_flags = Flags }.

remote_load_code(Module, Node) ->
  {_, Binary, Filename} = code:get_object_code(Module),
  rpc:call(Node, code, load_binary, [Module, Filename, Binary]).

init(Parent, State0) ->
  process_flag(trap_exit, true),
  application:start(cecho),
  ok = cecho:cbreak(),
  ok = cecho:noecho(),
  ok = cecho:curs_set(?ceCURS_INVISIBLE),
  ok = cecho:keypad(?ceSTDSCR, true),
  % keep track of the max screen size so we can resize if necessary
  MaxYX = cecho:getmaxyx(),
  State1 = State0#state { maxyx = MaxYX },

  State2 = init_callback(State1),
  print_nodeinfo(State2),
  Parent ! continue,
  self() ! time_update,
  loop(Parent, State2).

init_callback(State) ->
  case (State#state.callback):init(State#state.node) of
    {ok, {Columns, DefaultSort}, CBState}
      when DefaultSort >= 0 andalso DefaultSort < length(Columns) ->
      % sorting is based on 0 indices so all columns can be sorted on
      State#state{ columns = Columns, cbstate = CBState, sort = DefaultSort };
    {ok, {Columns, _}, CBState} ->
      State#state{ columns = Columns, cbstate = CBState, sort = 0 }
  end.

print_nodeinfo(State) ->
  cecho:move(0, 0),
  cecho:hline($ , ?MAX_HLINE),
  {Mj, Md, Mi} = State#state.os_version,
  OsVers = lists:concat([Mj,".",Md,".",Mi]),
  cecho:mvaddstr(0, 0, io_lib:format("Node: ~p ",[State#state.node])),
  case State#state.connected of
    false -> cecho:addstr("(Disconnected)");
    true -> cecho:addstr("(Connected)")
  end,
  Head = io_lib:format(" (~s/~s) ~p (~p ~s)~s",
                       [State#state.otp_version,
                        State#state.erts_version, State#state.os_fam,
                        State#state.os, OsVers, flags2str(State#state.node_flags)]),
  ok = cecho:addstr(lists:flatten(Head)).

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

loop(Parent, #state{ connected = false } = State) ->
  receive
    {nodeup, Node} when Node == State#state.node ->
      remote_load_code(State#state.remote_module, State#state.node),
      loop(Parent, fetch_and_update(State#state{ connected = true }, false));
    _ ->
      loop(Parent, State)
  end;
loop(Parent, State) ->
  receive
    time_update ->
      loop(Parent, fetch_and_update(State, false));
    force_update ->
      loop(Parent, fetch_and_update(State, true));
    {sort, N} when is_integer(N) ->
      State2 = update_sort_screen(State, N),
      loop(Parent, State2);
    {sort, Direction} ->
      State2 =
        case Direction of
          next -> update_sort_screen(State, State#state.sort + 1);
          prev -> update_sort_screen(State, State#state.sort - 1)
        end,
      loop(Parent, State2);
    reverse_sort ->
      State2 = fetch_and_update(State#state{ reverse_sort = (not State#state.reverse_sort) }, true),
      loop(Parent, State2);
    {'EXIT', Parent, _} ->
      ok
  end.

fetch_and_update(State, IsForced) ->
  case entop_net:fetch_data(State#state.node, State#state.remote_module) of
    {_Time, {badrpc, nodedown}} ->
      NState = State#state{ connected = false },
      print_nodeinfo(NState),
      cecho:refresh(),
      erlang:spawn_link(entop_net, reconnect, [self(), State#state.node]),
      NState;
    {_Time, {badrpc, {'EXIT', {undef, _}}}}->
      remote_load_code(State#state.remote_module, State#state.node),
      fetch_and_update(State, IsForced);
    {Time, {ok, HeaderData, RowDataList}} ->
      State2 = update_screen(Time, HeaderData, RowDataList, State),
      if not IsForced -> erlang:send_after(State2#state.interval, self(), time_update);
         true -> ok
      end,
      State2
  end.

update_sort_screen(State, N) ->
  case N >= 0 andalso N < length(State#state.columns) of
    true -> fetch_and_update(State#state{ sort = N }, true);
    false -> State
  end.

update_screen(Time, HeaderData, RowDataList, State0) ->
  % check to see if the screen has changed and if it has resize it
  NewMaxYX = {MaxY, MaxX} = cecho:getmaxyx(),
  State1 =
    case NewMaxYX =:= State0#state.maxyx of
      true -> State0;
      false ->
        % then resize the columns if it has
        {ok, Columns} =
          (State0#state.callback):resize(MaxX, State0#state.cbstate),
        State0#state{columns = Columns}
    end,

  print_nodeinfo(State1),
  draw_title_bar(State1),
  print_showinfo(State1, Time),
  {Headers, State2} = process_header_data(HeaderData, State1),
  lists:foldl(fun(Header, Y) ->
                  cecho:hline($ , ?MAX_HLINE),
                  cecho:mvaddstr(Y, 0, Header), Y + 1
              end, 1, Headers),
  {RowList, State3} = process_row_data(RowDataList, State2),
  SortedRowList = sort(RowList, State3),
  StartY = (MaxY-(MaxY-8)),
  lists:foreach(fun(N) -> cecho:move(N, 0), cecho:hline($ , ?MAX_HLINE) end, lists:seq(StartY, MaxY)),
  update_rows(SortedRowList, State3#state.columns, StartY, MaxY),
  cecho:refresh(),
  State3.

draw_title_bar(State) ->
  cecho:move(7, 0),
  cecho:attron(?ceA_REVERSE),
  cecho:hline($ , ?MAX_HLINE),
  draw_title_bar(State#state.columns, 0),
  cecho:attroff(?ceA_REVERSE).

draw_title_bar([], _) -> ok;
draw_title_bar([{Title, Width, Options}|Rest], Offset) ->
  Align = proplists:get_value(align, Options, left),
  cecho:mvaddstr(7, Offset, string:Align(Title, Width)++" "),
  draw_title_bar(Rest, Offset + Width + 1).

print_showinfo(State, RoundTripTime) ->
  cecho:move(6, 0),
  cecho:hline($ , ?MAX_HLINE),
  ColName = element(1,lists:nth(State#state.sort+1, State#state.columns)),
  SortName = if State#state.reverse_sort -> "Descending"; true -> "Ascending" end,
  Showing = io_lib:format("Interval ~pms, Sorting on ~p (~s), Retrieved in ~pms",
                          [State#state.interval, ColName, SortName, RoundTripTime div 1000]),
  cecho:mvaddstr(6, 0, lists:flatten(Showing)).

process_header_data(HeaderData, State) ->
  {ok, Headers, NCBState} = (State#state.callback):header(HeaderData, State#state.cbstate),
  {Headers, State#state{ cbstate = NCBState }}.

process_row_data(RowDataList, State) ->
  prd(RowDataList, State, {[], dict:new()}).

prd([], State, {Acc, NLRD}) ->
  {Acc, State#state{last_reductions = NLRD}};
prd([RowData|Rest], #state{last_reductions = LRD} = State, FullAcc = {Acc, LRDAcc}) ->
  Pid = proplists:get_value(pid, RowData),
  LastReductions = case dict:find(Pid, LRD) of
                     error -> 0;
                     {ok, N} -> N
                   end,
  case (State#state.callback):row(RowData, LastReductions, State#state.cbstate) of
    {ok, skip, NCBState} ->
      prd(Rest, State#state{ cbstate = NCBState }, FullAcc);
    {ok, Row, NCBState} ->
      NReds = (State#state.callback):row_reductions(Row),
      prd(Rest, State#state{ cbstate = NCBState }, {[Row|Acc], dict:store(Pid, NReds, LRDAcc)})
  end.

sort(ProcList, State) ->
  Sorted = lists:keysort(State#state.sort+1, ProcList),
  case State#state.reverse_sort of
    true ->
      lists:reverse(Sorted);
    false ->
      Sorted
  end.

update_rows(ProcValuesList, _, LineNumber, Max)
  when LineNumber == Max orelse ProcValuesList == [] ->
  ok;
update_rows([RowValues|Rest], Columns, LineNumber, Max) ->
  update_row(tuple_to_list(RowValues), Columns, LineNumber, 0),
  update_rows(Rest, Columns, LineNumber + 1, Max).

update_row(R, C, _, _)
  when R == [] orelse C == [] ->
    ok;
update_row([RowColValue|Rest], ColOpts, LineNumber, Offset)
  when is_function(RowColValue) ->
    update_row([RowColValue()|Rest], ColOpts, LineNumber, Offset);
update_row([RowColValue|Rest], [{_,Width,Options}|RestColumns],
           LineNumber, Offset) ->
  StrColVal = if is_list(RowColValue) -> RowColValue;
                 true -> lists:flatten(io_lib:format("~1000p",[RowColValue]))
              end,
  Aligned = case proplists:get_value(align, Options) of
              right ->
                string:right(StrColVal, Width);
              _ ->
                string:left(StrColVal, Width)
            end,
  cecho:mvaddstr(LineNumber, Offset, Aligned),
  update_row(Rest, RestColumns, LineNumber, Offset+Width+1).
