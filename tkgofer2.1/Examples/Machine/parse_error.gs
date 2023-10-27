-- parse error function suitable for use with generated scanners

parse_error state input
	= error ("Parse error (state " ++ show state ++ ") in\n"
		 ++ case input of {[] -> "END"; _ -> echo_input input})

echo_input ((_,_,(line,s)):input)
	= "line " ++ show line ++ " at\n" ++
	   s ++ head (lines (concat (map getstring input)))
	where getstring (_,_,(_,s)) = s
