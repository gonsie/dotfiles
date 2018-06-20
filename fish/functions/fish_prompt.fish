function fish_prompt --description 'Write out the prompt'
	set -l last_status $status

	# Just calculate these once, to save a few cycles when displaying the prompt
	if not set -q __fish_prompt_hostname
		set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
	end

	if not set -q __fish_prompt_color_normal
		set -g __fish_prompt_color_normal (set_color normal)
	end

	if not set -q -g __fish_classic_git_functions_defined
		set -g __fish_classic_git_functions_defined

		function __fish_repaint_user --on-variable fish_color_user --description "Event handler, repaint when fish_color_user changes"
			if status --is-interactive
				set -e __fish_prompt_user
				commandline -f repaint ^/dev/null
			end
		end

		function __fish_repaint_host --on-variable fish_color_host --description "Event handler, repaint when fish_color_host changes"
			if status --is-interactive
				set -e __fish_prompt_host
				commandline -f repaint ^/dev/null
			end
		end

		function __fish_repaint_status --on-variable fish_color_status --description "Event handler; repaint when fish_color_status changes"
			if status --is-interactive
				set -e __fish_prompt_status
				commandline -f repaint ^/dev/null
			end
		end
	end

	set -l delim ' >'

	switch $USER

	case root

		if not set -q __fish_prompt_color_cwd
			if set -q fish_color_cwd_root
				set -g __fish_prompt_color_cwd (set_color $fish_color_cwd_root)
			else
				set -g __fish_prompt_color_cwd (set_color $fish_color_cwd)
			end
		end

	case '*'

		if not set -q __fish_prompt_color_cwd
			set -g __fish_prompt_color_cwd (set_color $fish_color_cwd)
		end

	end

	set -l prompt_status
	if test $last_status -ne 0
		if not set -q __fish_prompt_color_status
			set -g __fish_prompt_color_status (set_color $fish_color_status)
		end
		set prompt_status "$__fish_prompt_color_status [$last_status]$__fish_prompt_color_normal"
	end

	if not set -q __fish_prompt_color_user
		set -g __fish_prompt_color_user (set_color $fish_color_user)
	end
	if not set -q __fish_prompt_color_host
		set -g __fish_prompt_color_host (set_color $fish_color_host)
	end

        if not set -q __fish_prompt_color_git
                set -g __fish_prompt_color_git (set_color green)
        end

        if not set -q __fish_prompt_color_time
                set -g __fish_prompt_color_time (set_color ffaf5f)
        end

        if set -q VIRTUAL_ENV
                if not set -q __fish_prompt_color_venv
                        set -g __fish_prompt_color_venv (set_color -b blue white)
                end
                set prompt_venv " $__fish_prompt_color_venv("(basename "$VIRTUAL_ENV")")$__fish_prompt_color_normal"
        end

	echo -n -s "$__fish_prompt_color_time" (date +%T) " $__fish_prompt_color_user" "$USER" "$__fish_prompt_color_normal" @ "$__fish_prompt_color_host" "$__fish_prompt_hostname" "$__fish_prompt_color_normal" ' ' "$__fish_prompt_color_cwd" (basename (prompt_pwd)) "$__fish_prompt_color_git" (__fish_git_prompt) "$__fish_prompt_color_normal" "$prompt_venv" "$prompt_status" "$delim" ' '
end
