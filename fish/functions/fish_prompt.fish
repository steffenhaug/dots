function fish_prompt;
    # Record whether the last command failed before we 
    # start running commands.
    set -l laststatus $pipestatus

    # Print my name.
    set_color bryellow
    printf "%s" (whoami) 

    # Print folder and git info.
    set_color yellow
    printf " %s" (prompt_pwd)
    fish_vcs_prompt
    printf " "

    # Indicate if the last command executed succsfully.
    __st_print_status $laststatus
end

function __st_print_status;
    # If the last command didnt exit succesfully (return 0) we
    # print a red fish that is running away.
    if test 0 -eq $argv[1]
        set_color brgreen
        printf "%s " "><>"
    else
        set_color brred
        printf "%s " "<><"
    end
end
