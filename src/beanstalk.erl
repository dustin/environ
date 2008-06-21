-module(beanstalk).

-export([connect/2]).

-export([put/2]).
-export([use/2]).
-export([reserve/1]).
-export([delete/2]).
-export([release/2]).
-export([bury/2]).
-export([watch/2]).
-export([ignore/2]).
-export([peek/2]).
-export([peek_ready/1]).
-export([peek_delayed/1]).
-export([peek_buried/1]).
-export([kick/2]).
-export([stats_job/2]).
-export([stats_tube/2]).
-export([stats/1]).
-export([list_tubes/1]).
-export([list_tube_used/1]).
-export([list_tubes_watched/1]).

-import(beanstalk_job, [id/1, priority/1, delay/1, ttr/1]).


connect(Host, Port) ->
  gen_tcp:connect(Host, Port, [binary, {packet, 0}]).

put(Body, Socket) when is_list(Body); is_binary(Body) ->
  beanstalk:put(beanstalk_job:new(Body), Socket);
put(Job, Socket) ->
  Body = beanstalk_job:body(Job),
  send_command({put, priority(Job), delay(Job), ttr(Job), size_of(Body)}, Socket),
  gen_tcp:send(Socket, Body),
  gen_tcp:send(Socket, "\r\n"),
  process_int(<<"INSERTED">>, inserted, receive_response(Socket)).

use(Tube, Socket) ->
  send_command({use, Tube}, Socket),
  process_using(receive_response(Socket)).

reserve(Socket) ->
  send_command("reserve\r\n", Socket),
  process(<<"DEADLINE_SOON\r\n">>, deadline_soon,
  process_job(<<"RESERVED">>, reserved, receive_response(Socket))).

delete(Job, Socket) when is_list(Job) ->
  delete(beanstalk_job:id(Job), Socket);
delete(ID, Socket) when is_integer(ID) ->
  send_command({delete, ID}, Socket),
  process(<<"DELETED\r\n">>, deleted,
  process_not_found(receive_response(Socket))).

release(Job, Socket) ->
  send_command({release, id(Job), priority(Job), delay(Job)}, Socket),
  process(<<"RELEASED\r\n">>, released,
  process_buried(process_not_found(receive_response(Socket)))).

bury(Job, Socket) ->
  send_command({bury, id(Job), priority(Job)}, Socket),
  process_buried(process_not_found(receive_response(Socket))).

watch(Tube, Socket) ->
  send_command({watch, Tube}, Socket),
  process_watching(receive_response(Socket)).

ignore(Tube, Socket) ->
  send_command({ignore, Tube}, Socket),
  process(<<"NOT_IGNORED\r\n">>, not_ignored,
  process_watching(receive_response(Socket))).

peek(ID, Socket) when is_integer(ID) ->
  send_command({peek, ID}, Socket),
  receive_peek_response(Socket).

peek_ready(Socket) ->
  send_command("peek-ready\r\n", Socket),
  receive_peek_response(Socket).

peek_delayed(Socket) ->
  send_command("peek-delayed\r\n", Socket),
  receive_peek_response(Socket).

peek_buried(Socket) ->
  send_command("peek-buried\r\n", Socket),
  receive_peek_response(Socket).

kick(Bound, Socket) when is_integer(Bound) ->
  send_command({kick, Bound}, Socket),
  process_int(<<"KICKED">>, kicked, receive_response(Socket)).

stats_job(ID, Socket) ->
  send_command({"stats-job", ID}, Socket),
  process_yaml(process_not_found(receive_response(Socket))).

stats_tube(Tube, Socket) ->
  send_command({"stats-tube", Tube}, Socket),
  process_yaml(process_not_found(receive_response(Socket))).

stats(Socket) ->
  send_command("stats\r\n", Socket),
  process_yaml(receive_response(Socket)).

list_tubes(Socket) ->
  send_command("list-tubes\r\n", Socket),
  process_yaml(receive_response(Socket)).

list_tube_used(Socket) ->
  send_command("list-tube-used\r\n", Socket),
  process_using(receive_response(Socket)).

list_tubes_watched(Socket) ->
  send_command("list-tubes-watched\r\n", Socket),
  process_yaml(receive_response(Socket)).


receive_peek_response(Socket) ->
  process_job(<<"FOUND">>, found, process_not_found(receive_response(Socket))).

process_watching(Response) ->
  process_int(<<"WATCHING">>, watching, Response).

process_buried(Response) ->
  process(<<"BURIED\r\n">>, buried, Response).

process_not_found(Response) ->
  process(<<"NOT_FOUND\r\n">>, not_found, Response).

process_using(Response) ->
  process_prefixed(<<"USING">>, using, fun binary_to_list/1, Response).

process_yaml({ok, <<"OK ", Bin/bytes>>}) ->
  {DataLength, <<"\r\n", Rem/bytes>>} = binary_take_int(Bin),
  {ok, yaml_parse(element(1, split_binary(Rem, DataLength)))};
process_yaml(Response) ->
  Response.

process_job(Prefix, Atom, Response) ->
  process_prefixed(Prefix, Atom, fun process_job_data/1, Response).

process_job_data(Bin) ->
  {ID, <<" ", Bin2/bytes>>} = binary_take_int(Bin),
  {_BodyLength, <<"\r\n", Body/bytes>>} = binary_take_int(Bin2),
  beanstalk_job:new(ID, Body).

process_int(Prefix, Atom, Response) ->
  process_prefixed(Prefix, Atom, fun binary_to_integer/1, Response).

process_prefixed(Prefix, Atom, Fun, Response={ok, Data}) ->
  case split_binary(Data, size(Prefix)) of
    {Prefix, <<" ", Rem/bytes>>} ->
      Bin = element(1, split_binary(Rem, size(Rem) - 2)), % remove \r\n
      {Atom, Fun(Bin)};
    _ ->
      Response
  end;
process_prefixed(_Prefix, _Atom, _Fun, Response) ->
  Response.

receive_response(Socket) ->
  receive
    {tcp, Socket, Response} ->
      process_errors({ok, Response});
    {tcp_closed, Socket} ->
      {error, socket_closed};
    {tcp_error, Socket, Reason} ->
      {error, Reason}
  end.

process_errors(Response) ->
  process(<<"OUT_OF_MEMORY\r\n">>, {error, out_of_memory},
  process(<<"INTERNAL_ERROR\r\n">>, {error, internal_error},
  process(<<"DRAINING\r\n">>, {error, draining},
  process(<<"BAD_FORMAT\r\n">>, {error, bad_format},
  process(<<"UNKNOWN_COMMAND\r\n">>, {error, unknown_command},
  Response))))).

process(Message, Term, _Response={ok, Message}) ->
  Term;
process(_Message, _Term, Response) ->
  Response.

binary_to_integer(Bin) when is_binary(Bin) ->
  list_to_integer(binary_to_list(Bin)).

binary_take_int(Bin) when is_binary(Bin) ->
  binary_take_int(Bin, []).

binary_take_int(<<C, Rem/bytes>>, Digits) when C >= $0, C =< $9 ->
  binary_take_int(Rem, [C|Digits]);
binary_take_int(Bin, Digits) ->
  {list_to_integer(lists:reverse(Digits)), Bin}.

send_command(Cmd, Socket) when is_list(Cmd) ->
  gen_tcp:send(Socket, Cmd);
send_command(Cmd, Socket) when is_tuple(Cmd) ->
  send_command(build_command(tuple_to_list(Cmd)), Socket).

build_command(Parts) when is_list(Parts) ->
  lists:reverse([$\n,$\r|lists:foldl(fun build_command/2, [], Parts)]).

build_command(Part, Message) when is_list(Message) ->
  lists:reverse(to_string(Part), case Message of [] -> []; _ -> [32|Message] end).

to_string(Int) when is_integer(Int) ->
  integer_to_list(Int);
to_string(Atom) when is_atom(Atom) ->
  atom_to_list(Atom);
to_string(List) ->
  List.

size_of(List) when is_list(List) ->
  length(List);
size_of(Bin) when is_binary(Bin) ->
  size(Bin).


yaml_parse(<<"---\n", Data/bytes>>) ->
  case Data of
    <<"- ", _/bytes>> ->
      yaml_parse_sequence(Data, []);
    _ ->
      yaml_parse_mapping(Data, [])
  end.

yaml_parse_sequence(Data, Sequence) when size(Data) =:= 0 ->
  lists:reverse(Sequence);
yaml_parse_sequence(<<"- ", Data/bytes>>, Sequence) ->
  {Value, MoreData} = binary_break_at($\n, Data),
  yaml_parse_sequence(MoreData, [Value|Sequence]).

yaml_parse_mapping(Data, Mapping) when size(Data) =:= 0 ->
  Mapping;
yaml_parse_mapping(Data, Mapping) ->
  {K, <<" ", Rest/bytes>>} = binary_break_at($:, Data),
  {V, MoreData} = binary_break_at($\n, Rest),
  yaml_parse_mapping(MoreData, [{K,V}|Mapping]).

binary_break_at(C, Data) when is_binary(Data) ->
  binary_break_at(C, Data, []).

binary_break_at(_C, Data, First) when is_binary(Data), size(Data) =:= 0 ->
  {lists:reverse(First), []};
binary_break_at(C, Data, First) when is_binary(Data) ->
  <<Head, Tail/bytes>> = Data,
  case Head of
    C ->
      {lists:reverse(First), Tail};
    _ ->
      binary_break_at(C, Tail, [Head|First])
  end.