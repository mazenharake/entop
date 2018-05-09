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

-module(entop_format).

-include_lib("cecho/include/cecho.hrl").

%% Module API
-export([init/1, header/2, row/3, row_reductions/1]).

%% Records
-record(state, { node = undefined, cache = [] }).

%% Defines
-define(KIB,(1024)).
-define(MIB,(?KIB*1024)).
-define(GIB,(?MIB*1024)).
-define(SECONDS_PER_MIN, 60).
-define(SECONDS_PER_HOUR, (?SECONDS_PER_MIN*60)).
-define(SECONDS_PER_DAY, (?SECONDS_PER_HOUR*24)).
-define(R(V,N), string:right(integer_to_list(V),N,$0)).

%% =============================================================================
%% Module API
%% =============================================================================
init(Node) ->
  ets:new(piddb, [set,named_table]),
  Columns = [
             {"Pid", 16, [{align, left}]},
             {"Process", 30, [{align, left}]},
             {"Current Function", 30, [{align, left}]},
             {"Status", 8, [{align, left}]},
             {"Reductions", 13, [{align, right}]},
             {"Reductions+", 13, [{align, right}]},
             {"Message Queue", 14, [{align, right}]},
             {"Memory", 15, [{align, right}]},
             {"Stack Size", 11, [{align, right}]},
             {"Heap Size", 12, [{align, right}]}
            ],
  {ok, {Columns, 6}, #state{ node = Node }}.

%% Header Callback
header(SystemInfo, State) ->
  Uptime = millis2uptimestr(element(1, proplists:get_value(uptime, SystemInfo, 0))),
  LocalTime = local2str(element(2, proplists:get_value(local_time, SystemInfo))),
  PingTime = element(1,timer:tc(net_adm, ping, [State#state.node])) div 1000,

  Row1Essentials = io_lib:format("Time: ~s, up for ~s, ~pms latency", [LocalTime, Uptime, PingTime]),
  Row1 = Row1Essentials
         ++ case proplists:get_value(cpu, SystemInfo, []) of
              [] ->
                [];
              CPUInfo ->
                CPUAvg1 = proplists:get_value(avg1, CPUInfo, 0.0),
                CPUAvg5 = proplists:get_value(avg5, CPUInfo, 0.0),
                CPUAvg15 = proplists:get_value(avg15, CPUInfo, 0.0),
                io_lib:format(", load average: ~.2f, ~.2f, ~.2f",
                              [CPUAvg1, CPUAvg5, CPUAvg15])
            end,

  PTotal = proplists:get_value(process_count, SystemInfo),
  RQueue = proplists:get_value(run_queue, SystemInfo),
  RedTotal = element(2,proplists:get_value(reduction_count, SystemInfo)),
  Row2 = io_lib:format("Processes: ~6w,  Run Queue: ~5w,  Reductions: ~9w namerpcs: ~B~n",
                       [PTotal, RQueue, RedTotal, ets:info(piddb,size)]),

  PMemUsed = mem2str(proplists:get_value(process_memory_used, SystemInfo)),
  PMemTotal = mem2str(proplists:get_value(process_memory_total, SystemInfo)),
  MemInfo = proplists:get_value(memory, SystemInfo),
  SystemMem = mem2str(proplists:get_value(system, MemInfo)),
  AtomMem = mem2str(proplists:get_value(atom, MemInfo)),
  BinMem = mem2str(proplists:get_value(binary, MemInfo)),
  CodeMem = mem2str(proplists:get_value(code, MemInfo)),
  EtsMem = mem2str(proplists:get_value(ets, MemInfo)),
  Row3 = io_lib:format("Process Memory: ~s total (~s used)", [PMemTotal, PMemUsed]),
  Row4 = io_lib:format("System Memory: ~s (Atom: ~s, Binary: ~s, Code: ~s, ETS: ~s)",
                       [SystemMem, AtomMem, BinMem, CodeMem, EtsMem]),
  Row5 = "",
  {ok, [ lists:flatten(Row) || Row <- [Row1, Row2, Row3, Row4, Row5] ], State}.

lookup_name(Props, State=#state{}) when is_list(Props) ->
  case proplists:get_value(registered_name, Props) of
    [] ->
      Pid = proplists:get_value(realpid, Props),
      case ets:lookup(piddb, Pid) of
        [{Pid,Val}] -> Val;
        [] ->
          Val = entop_net:lookup_name(State#state.node, entop_collector, Pid),
          ets:insert(piddb, {Pid, Val}),
          Val
      end;
    N ->
      N
  end.

format_name(Name) ->
  case Name of
    []          -> [];
    undefined   -> [];
    Name when is_atom(Name) ->
      atom_to_list(Name);
    {n,g,T}     -> lists:flatten(io_lib:format("g:~p",[T]));
    {n,l,T}     -> lists:flatten(io_lib:format("l:~p",[T]));
    Name        -> lists:flatten(io_lib:format("~p",[Name]))
  end.

%% Column Specific Callbacks
row([{pid,_},{realpid,_}|undefined], _LastReductions, State) ->
  {ok, skip, State};
row(ProcessInfo, LastReductions, State) ->
  Pid = proplists:get_value(pid, ProcessInfo),
  %% defer, only called if this row is rendered:
  ProcessName = fun() -> proc_name(ProcessInfo, State) end,
  CurrentFunction = proplists:get_value(current_function, ProcessInfo),
  Reductions = proplists:get_value(reductions, ProcessInfo, 0),
  ReductionsDiff = Reductions - LastReductions,
  Queue = proplists:get_value(message_queue_len, ProcessInfo, 0),
  Memory = proplists:get_value(memory, ProcessInfo, 0),
  StackSize = proplists:get_value(stack_size, ProcessInfo, 0),
  TotalHeapSize = proplists:get_value(total_heap_size, ProcessInfo, 0),
  Status = proplists:get_value(status, ProcessInfo),
  {ok, {Pid, ProcessName, CurrentFunction, Status, Reductions, ReductionsDiff, Queue, Memory, StackSize, TotalHeapSize}, State}.

row_reductions({_Pid, _, _, _, Reductions, _, _, _, _, _} = _Row) ->
  Reductions.

proc_name(ProcessInfo, State) ->
  case format_name(lookup_name(ProcessInfo, State)) of
    [] -> initial_call(ProcessInfo);
    Name -> Name
  end.

initial_call(ProcessInfo) ->
  ProcessDict = proplists:get_value('dictionary', ProcessInfo, []),
  case proplists:get_value('$initial_call', ProcessDict, []) of
    [] -> proplists:get_value(initial_call, ProcessInfo);
    Call -> Call
  end.

mem2str(Mem) ->
  if Mem > ?GIB -> io_lib:format("~.1fG",[Mem/?GIB]);
     Mem > ?MIB -> io_lib:format("~.1fM",[Mem/?MIB]);
     Mem > ?KIB -> io_lib:format("~.1fK",[Mem/?KIB]);
     Mem >= 0 -> io_lib:format("~.1fb",[Mem/1.0])
  end.

millis2uptimestr(Millis) ->
  SecTime = Millis div 1000,
  Days = ?R(SecTime div ?SECONDS_PER_DAY,3),
  Hours = ?R((SecTime rem ?SECONDS_PER_DAY) div ?SECONDS_PER_HOUR,2),
  Minutes = ?R(((SecTime rem ?SECONDS_PER_DAY) rem ?SECONDS_PER_HOUR) div ?SECONDS_PER_MIN, 2),
  Seconds = ?R(((SecTime rem ?SECONDS_PER_DAY) rem ?SECONDS_PER_HOUR) rem ?SECONDS_PER_MIN, 2),
  io_lib:format("~s:~s:~s:~s",[Days,Hours,Minutes,Seconds]).

local2str({Hours,Minutes,Seconds}) ->
  io_lib:format("~s:~s:~s",[?R(Hours,2),?R(Minutes,2),?R(Seconds,2)]).
