-module(eqsms).
-export([send_sms/3, send_sms/5,
         send_isms/2, send_isms/4,
         send_multi_sms/2, send_multi_sms/4
        ]).

-include_lib("eutil/include/eutil.hrl").

send_sms(NationCode, Mobile, Msg) ->
    {ok, Sdkappid} = application:get_env(eqsms, sdkappid),
    {ok, Appkey} = application:get_env(eqsms, appkey),
    send_sms(Sdkappid, Appkey, NationCode, Mobile, Msg).

send_sms(Sdkappid, Appkey, NationCode, Mobile, Msg) ->
    Host = <<"https://yun.tim.qq.com/">>,
    Path = <<"v5/tlssmssvr/sendsms">>,
    Random = eutil:to_binary(rand:uniform(10000)),
    URL = hackney_url:make_url(Host, Path, [{<<"sdkappid">>, Sdkappid}, {<<"random">>, Random}]),
    Time = erlang:system_time(seconds),
    Sig = gen_sms_sig(Appkey, Random, Time, Mobile),
    Tel = #{nationcode => eutil:to_binary(NationCode), mobile => eutil:to_binary(Mobile)},
    Payload = #{tel => Tel, type => 0, msg => Msg, sig => eutil:to_binary(Sig), time => Time},
    Result = eutil:http_post(URL, ?URLENCEDED_HEADS, eutil:json_encode(Payload)),
    handle_result(Mobile, Result).


send_isms(Tel, Msg) ->
    {ok, Sdkappid} = application:get_env(eqsms, sdkappid),
    {ok, Appkey} = application:get_env(eqsms, appkey),
    send_isms(Sdkappid, Appkey, Tel, Msg).

send_isms(Sdkappid, Appkey, Tel, Msg) ->
    Host = <<"https://yun.tim.qq.com/">>,
    Path = <<"v5/tlssmssvr/sendisms">>,
    Random = eutil:to_binary(rand:uniform(10000)),
    URL = hackney_url:make_url(Host, Path, [{<<"sdkappid">>, Sdkappid}, {<<"random">>, Random}]),
    Time = erlang:system_time(seconds),
    Sig = gen_isms_sig(Appkey, Random, Time, Tel),
    Payload = #{tel => eutil:to_binary(Tel), type => 0, msg => Msg, sig => eutil:to_binary(Sig), time => Time},
    Result = eutil:http_post(URL, ?URLENCEDED_HEADS, eutil:json_encode(Payload)),
    handle_result(Tel, Result).

send_multi_sms(Tels, Msg) ->
    {ok, Sdkappid} = application:get_env(eqsms, sdkappid),
    {ok, Appkey} = application:get_env(eqsms, appkey),
    send_multi_sms(Sdkappid, Appkey, Tels, Msg).

send_multi_sms(Sdkappid, Appkey, Tels, Msg) ->
    Host = <<"https://yun.tim.qq.com/">>,
    Path = <<"v5/tlssmssvr/sendmultisms2">>,
    Random = eutil:to_binary(rand:uniform(10000)),
    URL = hackney_url:make_url(Host, Path, [{<<"sdkappid">>, Sdkappid}, {<<"random">>, Random}]),
    Time = erlang:system_time(seconds),
    Sig = gen_multi_sig(Appkey, Random, Time, Tels),
    Payload = #{tel => Tels, type => 0, msg => Msg, sig => eutil:to_binary(Sig), time => Time},
    Result = eutil:http_post(URL, ?URLENCEDED_HEADS, eutil:json_encode(Payload)),
    handle_result(Tels, Result).


gen_sms_sig(Appkey, Random, Time, Mobile) ->
    %Str = hackney_url:qs([{appkey, Appkey}, {random, Random}, {time, Time}, {mobile, Mobile}]),
    Str = special_urlencode([{appkey, Appkey}, {random, Random}, {time, Time}, {mobile, Mobile}]),
    sha256_digest(Str).

gen_isms_sig(Appkey, Random, Time, Tel) ->
    %Str = hackney_url:qs([{appkey, Appkey}, {random, Random}, {time, Time}, {tel, Tel}]),
    Str = special_urlencode([{appkey, Appkey}, {random, Random}, {time, Time}, {tel, Tel}]),
    sha256_digest(Str).

gen_multi_sig(Appkey, Random, Time, Tels) ->
    Mobiles = [eutil:to_list(maps:get(mobile, Tel)) || Tel <- Tels],
    StrMobile = string:join(Mobiles, ","),
    %Str = hackney_url:qs([{appkey, Appkey}, {random, Random}, {time, Time}, {mobile, StrMobile}]),
    Str = special_urlencode([{appkey, Appkey}, {random, Random}, {time, Time}, {mobile, StrMobile}]),
    sha256_digest(Str).

handle_result(Mobile, Result) ->
    case maps:get(<<"result">>, Result) of
        ?SUCCESS_0 ->
            {ok, Result};
        _ ->
            error_logger:error_msg("eqsms send error, Mobile: ~p, Result: ~p", [Mobile, Result]),
            {error, Result}
    end.


sha256_digest(Str) ->
    Bin = crypto:hash(sha256, Str),
    eutil:binary_to_hexstr(Bin).


special_urlencode(Props) ->
    Pairs = lists:foldr(
              fun({K, V}, Acc) ->
                      [eutil:to_list(K) ++ "=" ++ eutil:to_list(V) | Acc]
              end, [], Props),
    string:join(Pairs, "&").
