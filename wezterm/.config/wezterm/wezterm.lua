local wezterm = require 'wezterm'
local config = {}

config = wezterm.config_builder()
-- do not start a login shell
config.default_prog = { "/bin/bash" }

if string.match(os.getenv("DESKTOP_SESSION"), "hyprland|niri") then
    config.window_decorations = "NONE"
else
    config.window_decorations = "TITLE"
    config.window_frame = {
        border_left_width = '1px',
        border_right_width = '1px',
        border_bottom_height = '1px',
        border_top_height = '1px',
        border_left_color = '#9f9690',
        border_right_color = '#9f9690',
        border_bottom_color = '#9f9690',
        border_top_color = '#9f9690',
    }
end

config.use_fancy_tab_bar = false
-- we don't need tabs if using sway, most likely we will
-- use say splits
if string.match(os.getenv("DESKTOP_SESSION"), "sway") then
   config.hide_tab_bar_if_only_one_tab = true
end

-- wezterm.gui is not available to the mux server, so take care to
-- do something reasonable when this config is evaluated by the mux
function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'Modus-Vivendi-Tinted'
  else
    return 'Modus-Operandi-Tinted'
  end
end

config.color_scheme = scheme_for_appearance(get_appearance())

config.enable_scroll_bar = false
config.term = "wezterm"
config.disable_default_key_bindings = true
--config.debug_key_events = true

config.leader = { key = 'b', mods = 'CTRL', timeout_milliseconds = 1000 }
keys = {
  -- move fullscreen to F11 like rest of gnome
  {
    key = 'F11',
    mods = 'NONE',
    action = wezterm.action.ToggleFullScreen,
  },
  {
    key = '|',
    mods = 'LEADER',
    action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  {
    key = '-',
    mods = 'LEADER',
    action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  {
    key = 'UpArrow',
    mods = 'META',
    action = wezterm.action.ActivatePaneDirection "Up",
  },
  {
    key = 'DownArrow',
    mods = 'META',
    action = wezterm.action.ActivatePaneDirection "Down",
  },
  {
    key = 'LeftArrow',
    mods = 'META',
    action = wezterm.action.ActivatePaneDirection "Left",
  },
  {
    key = 'RightArrow',
    mods = 'META',
    action = wezterm.action.ActivatePaneDirection "Right",
  },
  {
    key = 'LeftArrow',
    mods = 'LEADER',
    action = wezterm.action.AdjustPaneSize {"Left", 1},
  },
  {
    key = 'RightArrow',
    mods = 'LEADER',
    action = wezterm.action.AdjustPaneSize {"Right", 1},
  },
  {
    key = 'z',
    mods = 'LEADER',
    action = wezterm.action.TogglePaneZoomState,
  },
  {
    key = 'c',
    mods = 'LEADER',
    action = wezterm.action.SpawnTab "CurrentPaneDomain",
  },
  {
    key = ',',
    mods = 'LEADER',
    action = wezterm.action.PromptInputLine {
      description = 'Enter new name for tab',
      action = wezterm.action_callback(function(window, pane, line)
        -- ESC produces nil (abort), ENTER empty string (default value)
        if line then
          window:active_tab():set_title(line)
        end
      end),
    },
  },
  {
    key = 'mapped:&',
    mods = 'LEADER|SHIFT',
    action = wezterm.action.CloseCurrentTab { confirm = true },
  },
  {
    key = 'RightArrow',
    mods = 'META|SHIFT',
    action = wezterm.action.ActivateTabRelative(1),
  },
  {
    key = 'LeftArrow',
    mods = 'META|SHIFT',
    action = wezterm.action.ActivateTabRelative(-1),
  },
  {
    key = 'x',
    mods = 'LEADER',
    action = wezterm.action.CloseCurrentPane{ confirm=true },
  },
  {
    key = 'r',
    mods = 'LEADER',
    action = wezterm.action.ReloadConfiguration,
  },
  {
     key = '[',
    mods = 'LEADER',
    action = wezterm.action.ActivateCopyMode,
  },
  {
     key = 'e',
     mods = 'LEADER|CTRL',
     action = wezterm.action.ActivateCopyMode,
  },
  {
    key = 'p',
    mods = 'LEADER',
    action = wezterm.action.PasteFrom 'Clipboard',
  },
}

-- tmux emacs style copy paste

local copy_mode = wezterm.gui.default_key_tables().copy_mode
table.insert(copy_mode, { key = 'w', mods = 'ALT',
                          action = wezterm.action.Multiple {
                             wezterm.action.CopyTo 'ClipboardAndPrimarySelection',
                             wezterm.action.ClearSelection,
                             wezterm.action.CopyMode 'Close',
                          },
})

-- emacs stuff
table.insert(copy_mode, { key = 'Space',
                          mods = 'CTRL',
                          action = wezterm.action.CopyMode { SetSelectionMode = 'Cell' },
})
table.insert(copy_mode, { key = 'a', mods = 'CTRL',
                          action = wezterm.action.CopyMode 'MoveToStartOfLineContent' })
table.insert(copy_mode, { key = 'e', mods = 'CTRL',
                          action = wezterm.action.CopyMode 'MoveToEndOfLineContent' })
table.insert(copy_mode, { key = 'j', mods = 'CTRL',
                          action = wezterm.action.CopyMode 'Close' })

config.key_tables = {
   copy_mode = copy_mode,
}

-- set a shortcut leader-num for all tabs
for tab_num = 1, 8 do
   keys[#keys+1] = {
     key = tostring(tab_num),
     mods = 'LEADER',
     action = wezterm.action.ActivateTab(tab_num - 1)
   }
end

config.keys = keys

-- Not using gnome lately
--local wayland_gnome = require 'wayland_gnome'
--wayland_gnome.apply_to_config(config)

config.font_size = 12.0
config.font = wezterm.font_with_fallback {
   'Fira Code',
   'FiraCode Nerd Font',
   'Noto Color Emoji',
}

return config
