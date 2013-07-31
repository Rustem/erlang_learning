-record(state, {
		services=orddict:new(),
		jobs=orddict:new()}).

-record(service, {cosname, stub_module, regname}).