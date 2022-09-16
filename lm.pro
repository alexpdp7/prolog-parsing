% Simple lightweight markup language parser

% Currently, it only parses paragraphs, and is quite slow (100 lines -> 2s).
% Paragraphs must be terminated by explicit blank *and* empty lines (e.g. if a
% file doesn't end in a blank line, it will fail to be parsed).

% Parse interactively:
%
% ?- phrase(paragraphs(X), "hola\nfo o\n\n\na\n\n").
%   X = [p([tl(t("hola"),le("\n")),tl(t("fo o"),le("\n"))],bls("\n\n")),p([tl(t("a"),le("\n"))],bls("\n"))]
% ;  false.
%
% Parse a file:
%
% ?- use_module(library(pio)).
% ?- phrase_from_file(paragraphs(X), "filename").

:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(iso_ext)).

% a text line is text followed by a blank line
text_line(tl(T, LE)) --> text(T, disallowed(["\n"])), {T\=[]}, line_end(LE).

line_end(le("\n")) --> "\n".

% a paragraph is text lines, followed by blank lines
paragraph(p(TLS, BL)) --> text_lines(TLS), blank_lines(BL).

% text lines is either a single line, or a single line followed by text lines
text_lines([T]) --> text_line(T).
text_lines([T|TS]) --> text_line(T), text_lines(TS).

% blank lines is a non empty sequence, whose all its members are new lines.
blank_lines(bls(T)) --> seq(T), {T \=[], forall(member(M, T), M = '\n')}.

% paragraphs are either a paragraph followed by other paragraphs, or nothing.
paragraphs([P|PS]) --> paragraph(P), paragraphs(PS).
paragraphs([]) --> [].

% inline parsing

text(T) --> text(T, disallowed([])).

text([T|TS], disallowed(D)) --> sp(T, disallowed(D)), text(TS, disallowed(D)).
text([], _) --> [].

sp(inline(IB, T, IE), disallowed(D)) --> {matching_delims(IB, IE), \+memberchk(IB, D)}, begin_inline(bi(IB)), !, text(T, disallowed([IB, IE|D])), end_inline(ei(IE)).

sp(t(T), disallowed(D)) --> seq(T), {valid(T, disallowed(D))}, !.

valid(T, disallowed(D)) :- T\=[], forall(memberchk(DM, D), \+append([_, DM, _], T)).

begin_inline(bi(X)) --> {matching_delims(X, _)}, X.
end_inline(ei(X)) --> {matching_delims(_, X)}, X.

matching_delims("*", "*").
matching_delims("_", "_").
matching_delims("/", "/").
matching_delims("<", ">").
