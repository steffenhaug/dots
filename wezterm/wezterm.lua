local wezterm = require "wezterm"

-- Refresh with Ctrl-Shift-R
return {
    color_scheme = "Gruvbox Dark",
    use_fancy_tab_bar = false,
    hide_tab_bar_if_only_one_tab = true,
    -- Slashed Zero (0) in JetBrains Mono
    harfbuzz_features = {"zero"},

    -- https://wezfurlong.org/wezterm/config/default-keys.html
    -- for i in $(seq 1 9); do 
    --   gsettings set org.gnome.shell.keybindings switch-to-application-${i} [];
    -- done
    -- ^ Gnome default KBs block the default tab shortcuts.
    keys = {
        {key="s", mods="ALT", 
            action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
        {key="v", mods="ALT", 
            action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
        {key="h", mods="ALT", 
            action=wezterm.action{ActivatePaneDirection="Left"}},
        {key="j", mods="ALT", 
            action=wezterm.action{ActivatePaneDirection="Down"}},
        {key="k", mods="ALT", 
            action=wezterm.action{ActivatePaneDirection="Up"}},
        {key="l", mods="ALT", 
            action=wezterm.action{ActivatePaneDirection="Right"}},
    }
}
