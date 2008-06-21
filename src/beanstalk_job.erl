-module(beanstalk_job).

-export([new/1]).
-export([new/2]).
-export([id/1]).
-export([body/1]).
-export([priority/1]).
-export([delay/1]).
-export([ttr/1]).
-export([with/3]).


new(ID) when is_integer(ID) ->
  {beanstalk_job, [{id, ID}]};
new(Body) when is_list(Body); is_binary(Body) ->
  {beanstalk_job, [{body, Body}]}.

new(ID, Body) ->
  {beanstalk_job, [{id, ID}, {body, Body}]}.

id({beanstalk_job, Attrs}) ->
  proplists:get_value(id, Attrs).

body({beanstalk_job, Attrs}) ->
  proplists:get_value(body, Attrs).

priority({beanstalk_job, Attrs}) ->
  proplists:get_value(priority, Attrs, 0).

delay({beanstalk_job, Attrs}) ->
  proplists:get_value(delay, Attrs, 0).

ttr({beanstalk_job, Attrs}) ->
  proplists:get_value(ttr, Attrs, 60).

with(Key, Value, {beanstalk_job, Attrs}) ->
  {beanstalk_job, [{Key, Value}|proplists:delete(Key, Attrs)]}.
