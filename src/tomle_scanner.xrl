%% -*- mode: erlang -*-

Definitions.

ALPHA = [a-zA-Z]
ALNUM = [a-zA-Z0-9]
GRAPH = [!--/-Z\\^-~]
SPACING = (\s|\t|\f|\r)

YEAR = ([0-9][0-9][0-9][0-9])
MON = ([01][0-9])
DAY = ([0-3][0-9])
HOUR = ([0-2][0-9])
MIN = ([0-5][0-9])
SEC = {MIN}
TZD = (Z|(\+|-){HOUR}:{MIN})


Rules.

#[^\n]* : skip_token.
{SPACING}+ : skip_token.

"([^\"]|\\.)*" : {token, {string, TokenLine, TokenChars}}.
(true|false) : {token, {boolean, TokenLine, list_to_atom(TokenChars)}}.
{ALPHA}{GRAPH}* : {token, {identifier, TokenLine, TokenChars}}.
{YEAR}-{MON}-{DAY}T{HOUR}:{MIN}:{SEC}{TZD}? : {token, {datetime, TokenLine, TokenChars}}.
(\+|-)?[0-9]+ : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
(\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)? : {token, {number, TokenLine, list_to_float(TokenChars)}}.

= : {token, {'=', TokenLine}}.
\[ : {token, {'[', TokenLine}}.
] : {token, {']', TokenLine}}.
, : {token, {',', TokenLine}}.
\. : {token, {'.', TokenLine}}.
\n : {token, {nl, TokenLine}}.


Erlang code.
