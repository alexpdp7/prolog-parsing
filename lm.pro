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

% a text line is plain text followed by a blank line
text_line(tl(T, LE)) --> plain_text(T), line_end(LE).

% plain text is a sequence of anything, but not empty, and which does not
% contain a new line
plain_text(t(T)) --> seq(T), {T \= [], \+ member('\n', T)}.

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
