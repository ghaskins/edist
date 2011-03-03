-module(target_system).
-include_lib("kernel/include/file.hrl").
-export([install/3, get_ertsvsn/1, remove_all_files/2]).
-define(BUFSIZE, 8192).

%% Based on:
%% http://www.erlang.org/doc/system_principles/create_target.html#id59337

install(App, RootDir, TarFile) ->
    io:fwrite("Extracting ~s ...~n", [TarFile]),
    ok = extract_tar(TarFile, RootDir),
    RelFile = filename:join([RootDir, "releases", App ++ ".rel"]),
    {ok, [{release, {_Name, _Vsn}, {erts, ErtsVsn}, _Apps}]}
	= file:consult(RelFile),

    ErtsBinDirName = filename:join(["erts-" ++ ErtsVsn, "bin"]),
    ErtsBinDir = filename:join([RootDir, ErtsBinDirName]),
    BinDir = filename:join([RootDir, "bin"]),

    case {filelib:is_dir(BinDir), filelib:is_dir(ErtsBinDir)} of
	{true, true} -> ok;
	{_, _} ->
	    % This release did not package up its own ERTS, so lets copy
	    % our local install after verifying they are compatible
	    LocalRootDir = code:root_dir(),
	    {ok, LocalErtsVsn, _} = get_ertsvsn(LocalRootDir),
	    if
		LocalErtsVsn =/= ErtsVsn ->
		    throw("ERTS " ++ ErtsVsn ++
			      " required, but only " ++
			      LocalErtsVsn ++ " available");
		true -> ok
	    end,

	    lists:foreach(fun(Dir) ->
				  SrcDir = filename:join([LocalRootDir, Dir]),
				  DstDir = filename:join([RootDir, Dir]),
				  copy(SrcDir, DstDir)
			  end,
			  [ErtsBinDirName, "bin"]),
	    ok
    end,

    io:fwrite("Substituting in erl.src, start.src and start_erl.src to\n"
              "form erl, start and start_erl ...\n"),
    subst_src_scripts(["erl", "start", "start_erl"], ErtsBinDir, BinDir, 
                      [{"FINAL_ROOTDIR", RootDir}, {"EMU", "beam"}],
                      [preserve]),
    io:fwrite("Creating the RELEASES file ...\n"),
    create_RELEASES(RootDir, 
                    filename:join([RootDir, "releases", App])).

%% LOCALS 

get_ertsvsn(RootDir) ->
    StartErlDataFile = filename:join([RootDir, "releases", "start_erl.data"]),
    {ok, StartErlData} = read_txt_file(StartErlDataFile),
    [ErlVsn, RelVsn| _] = string:tokens(StartErlData, " \n"),
    {ok, ErlVsn, RelVsn}.

copy(SrcDir, DstDir) ->
    F = fun(SrcFile, Acc) ->
		BaseName = string:substr(SrcFile, length(SrcDir)+2),
		DstFile = filename:join([DstDir, BaseName]),
		ok = filelib:ensure_dir(DstFile),
		copy_file(SrcFile, DstFile, [preserve]),
		Acc
	end,
    ok = filelib:fold_files(SrcDir, ".*", true, F, ok).

%% extract_tar(TarFile, DestDir)
%%
extract_tar(TarFile, DestDir) ->
    erl_tar:extract(TarFile, [{cwd, DestDir}, compressed]).

create_RELEASES(DestDir, RelFileName) ->
    release_handler:create_RELEASES(DestDir, RelFileName ++ ".rel").

subst_src_scripts(Scripts, SrcDir, DestDir, Vars, Opts) -> 
    lists:foreach(fun(Script) ->
                          subst_src_script(Script, SrcDir, DestDir, 
                                           Vars, Opts)
                  end, Scripts).

subst_src_script(Script, SrcDir, DestDir, Vars, Opts) -> 
    subst_file(filename:join([SrcDir, Script ++ ".src"]),
               filename:join([DestDir, Script]),
               Vars, Opts).

subst_file(Src, Dest, Vars, Opts) ->
    {ok, Conts} = read_txt_file(Src),
    NConts = subst(Conts, Vars),
    write_file(Dest, NConts),
    case lists:member(preserve, Opts) of
        true ->
            {ok, FileInfo} = file:read_file_info(Src),
            file:write_file_info(Dest, FileInfo);
        false ->
            ok
    end.

%% subst(Str, Vars)
%% Vars = [{Var, Val}]
%% Var = Val = string()
%% Substitute all occurrences of %Var% for Val in Str, using the list
%% of variables in Vars.
%%
subst(Str, Vars) ->
    subst(Str, Vars, []).
subst([$%, C| Rest], Vars, Result) when $A =< C, C =< $Z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C| Rest], Vars, Result) when $a =< C, C =< $z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C| Rest], Vars, Result) when  C == $_ ->
    subst_var([C| Rest], Vars, Result, []);
subst([C| Rest], Vars, Result) ->
    subst(Rest, Vars, [C| Result]);
subst([], _Vars, Result) ->
    lists:reverse(Result).

subst_var([$%| Rest], Vars, Result, VarAcc) ->
    Key = lists:reverse(VarAcc),
    case lists:keysearch(Key, 1, Vars) of
        {value, {Key, Value}} ->
            subst(Rest, Vars, lists:reverse(Value, Result));
        false ->
            subst(Rest, Vars, [$%| VarAcc ++ [$%| Result]])
    end;
subst_var([C| Rest], Vars, Result, VarAcc) ->
    subst_var(Rest, Vars, Result, [C| VarAcc]);
subst_var([], Vars, Result, VarAcc) ->
    subst([], Vars, [VarAcc ++ [$%| Result]]).

copy_file(Src, Dest, Opts) ->
    {ok, InFd} = file:open(Src, [raw, binary, read]),
    {ok, OutFd} = file:open(Dest, [raw, binary, write]),
    {ok, _} = file:copy(InFd, OutFd),
    file:close(InFd),
    file:close(OutFd),
    case lists:member(preserve, Opts) of
        true ->
            {ok, FileInfo} = file:read_file_info(Src),
            file:write_file_info(Dest, FileInfo);
        false ->
            ok
    end.

write_file(FName, Conts) ->
    {ok, Fd} = file:open(FName, [write]),
    file:write(Fd, Conts),
    file:close(Fd).

read_txt_file(File) ->
    {ok, Bin} = file:read_file(File),
    {ok, binary_to_list(Bin)}.

remove_dir_tree(Dir) ->
    remove_all_files(".", [Dir]).

remove_all_files(Dir, Files) ->
    lists:foreach(fun(File) ->
                          FilePath = filename:join([Dir, File]),
                          {ok, FileInfo} = file:read_file_info(FilePath),
                          case FileInfo#file_info.type of
                              directory ->
                                  {ok, DirFiles} = file:list_dir(FilePath), 
                                  remove_all_files(FilePath, DirFiles),
                                  file:del_dir(FilePath);
                              _ ->
                                  file:delete(FilePath)
                          end
                  end, Files).

