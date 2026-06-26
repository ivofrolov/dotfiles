function fish_right_prompt
	if test -n "$INSIDE_EMACS"
		return
	end
	echo -n (fish_git_prompt)
end
