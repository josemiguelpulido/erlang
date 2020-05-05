-module(crypt).
-export([gcd/2, erasthotenes/1, rsa_keys/0]).

-export([test_encrypt/1]).

-export([start_alice/2, start_bob/0, init_handshake/3, handshake/1]).

gcd(A,B) when A rem B == 0 ->
    B;
gcd(A,B) when A rem B == 1 ->
   1;
gcd(A, B) ->
    gcd(B, A rem B).


erasthotenes(N) ->
    S = round(math:sqrt(N)),
    erasthotenes(S, 2, lists:seq(1,S)).
    
erasthotenes(N, K, L) when K < N ->
    P = [if X rem K == 0, X > K -> 0; true -> X end || X <- L],
    erasthotenes(N, K+1, P);
erasthotenes(N, K, L) ->
    [X || X <- L, X > 1].

                                     
rsa_keys() ->

    % RSA algorithm uses Euler's a^Phi(N) = 1 mod N,
    % as a^Phi(N)+1 = a mod N.
    % Constructs E*D = k*Phi(N) + 1,
    % equivalent to D being inverse of E mod Phi(N).
    % (E,N) is public key, (D,N) is private key

    % get a list of primes
    P = erasthotenes(10000),
    io:format("Erastothenes: ~p~n",[P]),

    % select 2 primes P1 and P2 at random
    [P1,P2|RestP] = [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- P])],

    %E = 3,
    %P1 =23,
    %P2 = 11,

    % compute N and Phi(N)
    N = P1 * P2,
    Phi = (P1-1)*(P2-1),

    % find E relatively prime to Phi(N) (to ensure a D exists)
    [E|_] = [ X || X <- RestP, gcd(X, Phi) == 1],

    % compute D as the inverse of E, mod Phi(N)    
    % (naive approach. More efficient via Extended Euclid)
    L = lists:seq(1,Phi-1),
    [D|_] = [X || {X,R} <- [{K, K*E rem Phi} || K <- L], 
                  R == 1, X /= E],
    % rsa keys
    io:format("E: ~w, N:~w, D: ~w~n",[E,N,D]),
    [E, N, D].


int_pow(N,M) ->
    int_pow(N,M,1).

int_pow(N,0,R) ->
    R;
int_pow(N,M,R) ->
    int_pow(N,M-1,N*R).

encrypt(Text, E) ->
    [int_pow(X,E) || X <- Text].

decrypt(Text, D, N) ->
    [int_pow(X,D) rem N || X <- Text].

test_encrypt(Text) ->

    % generate keys
    [E,N,D] = rsa_keys(),
    io:format("~p, ~p, ~p~n",[E, N, D]),

    % encrypt    
    io:format("Original text: ~p~n", [Text]),

    CText = encrypt(Text,E),
    io:format("Cyphertext: ~p~n", [CText]),

    % decrypt
    PText = decrypt(CText,D,N),
    io:format("Plaintext: ~p~n", [PText]).

    
init_handshake([E,N,D], Bob_Node, Msg) ->


    {bob, Bob_Node} ! {keys, [E,N], self()},

    receive 
        {keys, [Er, Nr], Pid} ->
            CText = encrypt(Msg, Er),
            io:format("message sent: ~p~n",[CText]),
            {bob, Bob_Node} ! {encrypted, CText, self()}
            %comm([E,N,D],[Er, Nr])
    end.

handshake([E,N,D]) ->
    
    receive 
        {keys, [Er, Nr], Pid} ->
            Pid ! {keys, [E,N], self()},
            comm([E,N,D],[Er, Nr])
    end.
        
comm([E,N,D], [Er, Nr]) ->

    receive 
        
        {encrypted, CText, Pid} ->
            PText = decrypt(CText, D, N),
            io:format("message received: ~p~n",[PText])
    end.
    
start_alice(Msg, Node) ->
    [E,N,D] = rsa_keys(),
    spawn(crypt, init_handshake, [[E,N,D],Node, Msg]).

start_bob() ->
    [E,N,D] = rsa_keys(),
    %register(bob, spawn(crypt, handshake, [[E,N,D]])).
    register(bob, spawn(fun() ->
                                handshake([E,N,D])
                        end)).

    
    
