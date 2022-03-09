# Homemade add_path while waiting for 3.2 on debian (:
function add_path -a DIR;
  set PATH $DIR $PATH;
end

add_path "$XDG_DATA_HOME"/cargo/bin
add_path "$XDG_DATA_HOME"/gcc-arm/bin
add_path "$HOME"/.local/bin
add_path "$HOME"/.local/bin/commander

alias nv=nvim
alias xa=exa
