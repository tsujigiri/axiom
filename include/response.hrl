-record(response, {
		status = 200                                  :: non_neg_integer(),
		headers = [{'Content-Type', <<"text/html">>}] :: [tuple()],
		body = <<>>                                   :: iodata(),
		req                                           :: cowboy_req:req()
}).
