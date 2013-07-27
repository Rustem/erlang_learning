-module(orb_utils).
-compile(export_all).

gen_stubs_skels(FName, OutDir) ->
		%% generates stubs an skeletons for particular IDL filename.
		case ic:gen(FName, [{outdir, OutDir}]) of
				ok -> ok;
				error ->
						io:format("Error while compiling idl.")
		end.

gen_impl_modules(FName, OutDir) ->
		case ic:gen(FName, [
						{outdir, OutDir},
						{be, erl_template}]) of

				ok -> generated;
				error -> io:format("Error while gen template impl modules.")
	
		end.

		