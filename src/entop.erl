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

-module(entop).

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

