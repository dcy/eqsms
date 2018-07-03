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
            %?ERROR_MSG("eqsms send error, Mobile: ~p, Result: ~p", [Mobile, Result]),
            error_logger:error_msg("eqsms send error, Mobile: ~p, Result: ~p", [Mobile, Result]),
            {error, Result}
    end.




-define(H, [16#6A09E667, 16#BB67AE85, 16#3C6EF372, 16#A54FF53A,
            16#510E527F, 16#9B05688C, 16#1F83D9AB, 16#5BE0CD19]).

-define(K, <<16#428A2F98:32/big-unsigned, 16#71374491:32/big-unsigned, 16#B5C0FBCF:32/big-unsigned,
            16#E9B5DBA5:32/big-unsigned, 16#3956C25B:32/big-unsigned, 16#59F111F1:32/big-unsigned,
            16#923F82A4:32/big-unsigned, 16#AB1C5ED5:32/big-unsigned, 16#D807AA98:32/big-unsigned,
            16#12835B01:32/big-unsigned, 16#243185BE:32/big-unsigned, 16#550C7DC3:32/big-unsigned,
            16#72BE5D74:32/big-unsigned, 16#80DEB1FE:32/big-unsigned, 16#9BDC06A7:32/big-unsigned,
            16#C19BF174:32/big-unsigned, 16#E49B69C1:32/big-unsigned, 16#EFBE4786:32/big-unsigned,
            16#0FC19DC6:32/big-unsigned, 16#240CA1CC:32/big-unsigned, 16#2DE92C6F:32/big-unsigned,
            16#4A7484AA:32/big-unsigned, 16#5CB0A9DC:32/big-unsigned, 16#76F988DA:32/big-unsigned,
            16#983E5152:32/big-unsigned, 16#A831C66D:32/big-unsigned, 16#B00327C8:32/big-unsigned,
            16#BF597FC7:32/big-unsigned, 16#C6E00BF3:32/big-unsigned, 16#D5A79147:32/big-unsigned,
            16#06CA6351:32/big-unsigned, 16#14292967:32/big-unsigned, 16#27B70A85:32/big-unsigned,
            16#2E1B2138:32/big-unsigned, 16#4D2C6DFC:32/big-unsigned, 16#53380D13:32/big-unsigned,
            16#650A7354:32/big-unsigned, 16#766A0ABB:32/big-unsigned, 16#81C2C92E:32/big-unsigned,
            16#92722C85:32/big-unsigned, 16#A2BFE8A1:32/big-unsigned, 16#A81A664B:32/big-unsigned,
            16#C24B8B70:32/big-unsigned, 16#C76C51A3:32/big-unsigned, 16#D192E819:32/big-unsigned,
            16#D6990624:32/big-unsigned, 16#F40E3585:32/big-unsigned, 16#106AA070:32/big-unsigned,
            16#19A4C116:32/big-unsigned, 16#1E376C08:32/big-unsigned, 16#2748774C:32/big-unsigned,
            16#34B0BCB5:32/big-unsigned, 16#391C0CB3:32/big-unsigned, 16#4ED8AA4A:32/big-unsigned,
            16#5B9CCA4F:32/big-unsigned, 16#682E6FF3:32/big-unsigned, 16#748F82EE:32/big-unsigned,
            16#78A5636F:32/big-unsigned, 16#84C87814:32/big-unsigned, 16#8CC70208:32/big-unsigned,
            16#90BEFFFA:32/big-unsigned, 16#A4506CEB:32/big-unsigned, 16#BEF9A3F7:32/big-unsigned,
            16#C67178F2:32/big-unsigned>>).

-define(ADD32(X, Y), (X + Y) band 16#FFFFFFFF).

sha256_digest(M) when is_binary(M) ->
    lists:flatten([io_lib:format("~8.16.0b", [V]) || V <- local_sha256(split_binary(sha256_pad(M), 64), ?H)]);
sha256_digest(Str) ->
    sha256_digest(list_to_binary(Str)).

rotate(V, Count) ->
    Rest = 32 - Count,
    <<Top:Rest/unsigned, Bottom:Count/unsigned>> = <<V:32/big-unsigned>>,
    <<New:32/big-unsigned>> = <<Bottom:Count/unsigned, Top:Rest/unsigned>>,
    New.

sha256_pad(M) ->
    Len = size(M),
    Len_bits = Len*8,
    Pad_bits = (Len + 8 + 1) rem 64,
    Pad = case Pad_bits of
              0 -> 0;
              _ -> (64 - Pad_bits) * 8
          end,
    list_to_binary([M, <<16#80:8, 0:Pad, Len_bits:64/big-unsigned>>]).

local_sha256_extend(W, 64) ->
    W;
local_sha256_extend(W, Count) ->
    Off1 = (Count - 15) * 4,
    Off2 = (Count - 2) * 4 - Off1 - 4,
    <<_:Off1/binary, Word1:32/big-unsigned, _:Off2/binary, Word2:32/big-unsigned, _/binary>> = <<W/binary>>,
    S0 = rotate(Word1, 7) bxor rotate(Word1, 18) bxor (Word1 bsr 3),
    S1 = rotate(Word2, 17) bxor rotate(Word2, 19) bxor (Word2 bsr 10),
    Off3 = (Count - 16) * 4,
    Off4 = (Count - 7) * 4 - Off3 - 4,
    <<_:Off3/binary, W16:32/big-unsigned, _:Off4/binary, W7:32/big-unsigned, _/binary>> = <<W/binary>>,
    Next = (W16 + S0 + W7 + S1) band 16#FFFFFFFF,
    local_sha256_extend(<<W/binary, Next:32/big-unsigned>>, Count+1).

local_sha256_loop(_W, Hashes, Next, 64) ->
    lists:map(fun({X, Y}) -> ?ADD32(X, Y) end, lists:zip(Hashes, Next));
local_sha256_loop(W, Hashes, [A, B, C, D, E, F, G, H], Count) ->
    S0 = rotate(A, 2) bxor rotate(A, 13) bxor rotate(A, 22),
    Maj = (A band B) bxor (A band C) bxor (B band C),
    T2 = ?ADD32(S0, Maj),
    S1 = rotate(E, 6) bxor rotate(E, 11) bxor rotate(E, 25),
    Ch = (E band F) bxor (((bnot E) + 1 + 16#FFFFFFFF) band G),
    Offset = Count * 4,
    <<_:Offset/binary, K:32/big-unsigned, _/binary>> = ?K,
    <<_:Offset/binary, Wval:32/big-unsigned, _/binary>> = <<W/binary>>,
    T1 = (H + S1 + Ch + K + Wval) band 16#FFFFFFFF,
    local_sha256_loop(W, Hashes, [?ADD32(T1, T2), A, B, C, ?ADD32(D, T1), E, F, G], Count+1).

local_sha256(M, Hashes) when is_binary(M) ->
    Words64 = local_sha256_extend(M, 16),
    local_sha256_loop(Words64, Hashes, Hashes, 0);
local_sha256({M, <<>>}, Hashes) ->
    local_sha256(M, Hashes);
local_sha256({M, T}, Hashes) ->
    local_sha256(split_binary(T, 64), local_sha256(M, Hashes)).

special_urlencode(Props) ->
    Pairs = lists:foldr(
              fun({K, V}, Acc) ->
                      [eutil:to_list(K) ++ "=" ++ eutil:to_list(V) | Acc]
              end, [], Props),
    string:join(Pairs, "&").
    
