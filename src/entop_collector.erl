%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(entop_collector).

-author('mazen.harake@erlang-solutions.com').

%% Module API
-export([get_data/0]).
-export([lookup_name/1]).

%% =============================================================================
%% Module API
%% =============================================================================
get_data() ->
    HeaderProplist = [{uptime, erlang:statistics(wall_clock)},
		      {local_time, calendar:local_time()},
		      {process_count, erlang:system_info(process_count)},
		      {run_queue, erlang:statistics(run_queue)},
		      {reduction_count, erlang:statistics(reductions)},
		      {process_memory_used, erlang:memory(processes_used)},
		      {process_memory_total, erlang:memory(processes)},
		      {memory, erlang:memory([system, atom, atom_used, binary, code, ets])}
		     ],
    HeaderProplist1 = HeaderProplist ++ case os_mon_started() of
        true ->
            [{cpu, [{avg1, cpu_sup:avg1() / 256}, {avg5, cpu_sup:avg5() / 256}, {avg15, cpu_sup:avg15() / 256}]}];
        false ->
            []
    end,
    Self = self(),
    ProcessesProplist =  [ [ {pid,erlang:pid_to_list(P)}, {realpid, P}  | process_info_items(P) ] ||
			     P <- erlang:processes(), P /= Self ],

    {ok, HeaderProplist1, ProcessesProplist}.

%% =============================================================================
%% Internal Functions
%% =============================================================================
process_info_items(P) ->
    erlang:process_info(P, [registered_name,
                            reductions,
                            message_queue_len,
                            heap_size,
                            stack_size,
                            total_heap_size,
                            memory,
                            dictionary,
                            initial_call,
                            current_function,
                            status]).

os_mon_started() ->
    [App || {os_mon, _, _} = App <- application:which_applications()] /= [].

lookup_name(Pid) when is_pid(Pid) ->
  case whereis(gproc) of
    undefined -> undefined;
    _ ->
      {gproc, Props} = gproc:info(Pid, gproc),
      case [ E || {E,_} <- Props, element(1,E)==n ] of
          [] -> undefined;
          [Ret] -> Ret
      end
  end.
