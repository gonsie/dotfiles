function fish_colors --description 'Show Colors'
        echo (set_color "--background=brblack") "*These colors are defined by the Terminal Application*" (set_color normal)
        echo -e (set_color black) black \t\t (set_color brblack) brblack
        echo -e (set_color red) red \t\t (set_color brred) brred
        echo -e (set_color green) green \t\t (set_color brgreen) brgreen
        echo -e (set_color yellow) yellow \t (set_color bryellow) bryellow
        echo -e (set_color blue) blue \t\t (set_color brblue) brblue
        echo -e (set_color magenta) magenta \t (set_color brmagenta) brmagenta
        echo -e (set_color cyan) cyan \t\t (set_color brcyan) brcyan
        echo -e (set_color white) white \t\t (set_color brwhite) brwhite
        echo -e (set_color brown) brown
        echo -e (set_color normal) normal
        echo (set_color "--background=brblack") "*These colors are defined in Fish (fish_color_x)*" (set_color normal)
        echo (set_color "$fish_color_autosuggestion") autosuggestion  $fish_color_autosuggestion (set_color normal)
        echo -e (set_color "$fish_color_command") command \t $fish_color_command (set_color normal)
        echo -e (set_color "$fish_color_comment") comment \t $fish_color_comment (set_color normal)
        echo -e (set_color "$fish_color_cwd") cwd \t\t $fish_color_cwd (set_color normal)
        echo -e (set_color "$fish_color_cwd_root") cwd_root \t $fish_color_cwd_root (set_color normal)
        echo -e (set_color "$fish_color_end") end \t\t $fish_color_end (set_color normal)
        echo -e (set_color "$fish_color_error") error \t\t $fish_color_error (set_color normal)
        echo -e (set_color "$fish_color_escape") escape \t $fish_color_escape (set_color normal)
        echo -e (set_color "$fish_color_history_current") history_current $fish_color_history_current (set_color normal)
        echo -e (set_color "$fish_color_host") host \t\t $fish_color_host (set_color normal)
        echo -e (set_color "$fish_color_match") match \t\t $fish_color_match (set_color normal)
        echo -e (set_color "$fish_color_normal") normal \t $fish_color_normal (set_color normal)
        echo -e (set_color "$fish_color_operator") operator \t $fish_color_operator (set_color normal)
        echo -e (set_color "$fish_color_param") param \t\t $fish_color_param (set_color normal)
        echo -e (set_color "$fish_color_quote") quote \t\t $fish_color_quote (set_color normal)
        echo -e (set_color "$fish_color_redirection") redirection \t $fish_color_redirection (set_color normal)
        echo -e (set_color "$fish_color_search_match") search_match \t $fish_color_search_match (set_color normal)
        echo -e (set_color "$fish_color_selection") selection \t $fish_color_selection (set_color normal)
        echo -e (set_color "$fish_color_status") status \t $fish_color_status (set_color normal)
        echo -e (set_color "$fish_color_user") user \t\t $fish_color_user (set_color normal)
        echo -e (set_color "$fish_color_valid_path") vaild_path \t $fish_color_valid_path (set_color normal)
end
