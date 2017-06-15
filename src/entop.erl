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
-module(entop).

-author('mazen.harake@erlang-solutions.com').

-include("entop.hrl").
-include_lib("cecho/include/cecho.hrl").

%% escript bits
-export([main/1]).

%% Application API
-export([start/1]).

-ifdef(random_module_available).
rand_seed() ->
  random:seed(os:timestamp()).
rand_uniform(N) ->
  random:uniform(N).
-else.
rand_seed() ->
  ok.
rand_uniform(N) ->
  rand:uniform(N).
-endif.

main([]) ->
  io:format("Usage: ./entop <TARGETNODE> [<COOKIE>]~n", []),
  halt(1);
main([Node]) ->
  main([Node,undefined]);
main([NodeIn,CookieIn]) ->
  % node and cookie should both be atoms
  Node = normalize_to_atom(NodeIn),
  Cookie = normalize_to_atom(CookieIn),
  case name_type(Node) of
    { error, improper_node_name } ->
      io:format("Nodename ~p is malformed, use form 'node@host'~n", [Node]),
      halt(101);
    {ok, NameType} ->
      case net_kernel:start(NameType) of
        {ok, _} ->
          case maybe_set_cookie(Node,Cookie) of
            ok ->
              case net_kernel:connect(Node) of
                true ->
                  ViewPid = entop_view:start(#state{ node = Node,
                                                     connected = true }),
                  control(ViewPid);
                false ->
                  io:format("Unable to connect to '~p', check nodename, cookie and network~n",[Node]),
                  halt(101)
              end;
            CE ->
              io:format("Cookie ~p is malformed, got error ~p~n", [Cookie,CE]),
              halt(101)
          end;
        NetError ->
          io:format("Couldn't start network with ~p, got error ~p~n",
                    [NameType, NetError]),
          halt(101)
      end
  end.

normalize_to_atom(L) when is_list(L) ->
  list_to_atom(L);
normalize_to_atom(A) when is_atom(A) ->
  A.

maybe_set_cookie(_, undefined) -> ok;
maybe_set_cookie(Node, Cookie) ->
  case erlang:set_cookie(Node,Cookie) of
    true -> ok;
    E -> E
  end.

name_type (Node) when is_atom(Node) ->
  name_type(atom_to_list(Node));
name_type (Node) when is_list(Node) ->
  case string:tokens(Node,"@") of
    [_] -> {error, improper_node_name};
    [N,H] ->
      T =
        case lists:member($.,H) of
          true -> longnames;
          false -> shortnames
        end,
      rand_seed(),
      R = rand_uniform(1000000),
      EntopName = list_to_atom("entop-" ++ integer_to_list(R) ++ "-" ++ N),
      {ok,[EntopName,T]}
  end.

%% =============================================================================
%% Application API
%% =============================================================================
start(Node) ->
    State = #state{ node = Node },
    case net_kernel:connect(Node) of
	true ->
	    ViewPid = entop_view:start(State#state{ connected = true }),
	    control(ViewPid);
	false ->
	    halt(101)
    end.

control(ViewPid) ->
    P = cecho:getch(),
    case P of
	N when N >= 49 andalso N =< 57 -> ViewPid ! {sort, N - 48}, control(ViewPid);
	$> -> ViewPid ! {sort, next}, control(ViewPid);
	$< -> ViewPid ! {sort, prev}, control(ViewPid);
	$r -> ViewPid ! reverse_sort, control(ViewPid);
	$q -> do_exit(ViewPid);
	3 -> do_exit(ViewPid); %Ctrl-C
	_ -> ViewPid ! force_update, control(ViewPid)
    end.

do_exit(ViewPid) ->
    exit(ViewPid, normal), 
    application:stop(cecho),
    halt().

