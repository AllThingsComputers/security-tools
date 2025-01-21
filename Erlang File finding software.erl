% Erlang File Finder
% https://www.erlang.org/faq/getting_started.html
% https://www.erlang.org/doc/man/file.html#list_dir_all-1

-module(file_finder).
-export([start/0, find_file/3]).

% Start function to list all files in a directory
start() ->
    io:format("Listing all files in the directory...~n"),
    case file:list_dir_all("C:/Users/./.") of
        {ok, Files} ->
            io:format("Files found:~n"),
            lists:foreach(fun(F) -> io:format("~p~n", [F]) end, Files);
        {error, Reason} ->
            io:format("Failed to list directory: ~p~n", [Reason])
    end.

% Function to find a specific file in a directory based on rules
find_file(Filename, Dir, Rules) ->
    case file:list_dir_all(Dir) of
        {ok, Files} ->
            Filtered = lists:filter(fun(File) -> matches_rules(File, Filename, Rules) end, Files),
            case Filtered of
                [Match | _] -> {ok, Match};
                [] -> {error, not_found}
            end;
        {error, Reason} -> {error, Reason}
    end.

% Helper function to check if a file matches the given rules
matches_rules(File, Filename, Rules) ->
    case filename:extension(File) of
        Ext when Ext == filename:extension(Filename) ->
            lists:any(fun(Rule) -> Rule(File) end, Rules);
        _ -> false
    end.
