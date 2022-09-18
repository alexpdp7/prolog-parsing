% Sample parsing of some AsciiDoc features
% This one currently only works on SWI Prolog
%
% Differences from AsciiDoctor:
%
% * _*a*_ is not correctly handled by AsciiDoctor https://asciidoc.zulipchat.com/#narrow/stream/335219-asciidoc-lang/topic/Clarification.20on.20constrained.20formatting.20marks/near/299416275

:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).

punct(pu(",")) --> ",".
punct(pu(";")) --> ";".
punct(pu("\"")) --> "\"".
punct(pu(".")) --> ".".
punct(pu("?")) --> "?".
punct(pu("!")) --> "!".

space(sp(" ")) --> " ".

formatting_mark(fm("*")) --> "*".
formatting_mark(fm("_")) --> "_".
formatting_mark(fm("`")) --> "`".
formatting_mark(fm("#")) --> "#".
formatting_mark(fm("~")) --> "~".

pre_constrained_formatting_mark(pre_cfm(X)) --> space(X).
pre_constrained_formatting_mark(pre_cfm(bl)) --> [bl].

post_constrained_formatting_mark(post_cfm(X)) --> space(sp([X])).
post_constrained_formatting_mark(post_cfm(X)) --> punct(pu([X])).
post_constrained_formatting_mark(post_cfm(el)) --> [el].

constrained_formatting_mark([Pre, cfm(F, T, F)]), [Post] -->
    pre_constrained_formatting_mark(Pre),
    formatting_mark(F),
    {F=fm(FM)},
    nested_line_parts(T, FM),
    {not_wrapped_in_spaces(T)},
    formatting_mark(F),
    post_constrained_formatting_mark(post_cfm(Post)).

not_wrapped_in_spaces(X) :- not_prefixed_by_spaces(X), reverse(X, RX), not_prefixed_by_spaces(RX).
not_prefixed_by_spaces([txt([bl])|X]) :- !, not_prefixed_by_spaces(X).
not_prefixed_by_spaces([txt([' '])|_]) :- !, fail.
not_prefixed_by_spaces(_).

% this is a hack to surround the content of a constrained formatting mark in be-el
nested_line_parts(X, F, B, A) :- append([B1, F, B2], B), append([[bl], B1, [el], F, B2], NB), line_parts(X, NB, A).

text(txt(T)) --> string(T), {length(T,X), X =\= 0}.

line_part(X) --> constrained_formatting_mark(X), !.
line_part(X) --> text(X), !.

line_parts([X|XS]) --> line_part(X), line_parts(XS).
line_parts([]) --> [].

parse_line(X, Y) :- append([[bl], X, [el]], XW), phrase(line_parts(Y), XW).
