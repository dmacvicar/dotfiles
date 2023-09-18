local wezterm = require 'wezterm'
local config = {}
config = wezterm.config_builder()

config.use_fancy_tab_bar = false

config.colors = {
  -- Modus Operandi Tinded
  -- https://protesilaos.com/emacs/modus-themes-colors
  ansi = {
    "#000000", -- black
    "#A60000", -- maroon
    "#006800", -- green
    "#6F5500", -- olive
    "#0031A9", -- navy
    "#721045", -- purple
    "#00538B", -- teal
    "#DFD6CD", -- silver
  },
  brights = {
    "#585858", -- grey
    "#972500", -- red
    "316500", -- lime
    "#884900", -- yellow
    "#354FCF", -- blue
    "#531AB6", -- fuchsia
    "#005A5F", -- aqua
    "#FBF7F0", -- white
  },

  cursor_bg = '#dfa0f0',
  cursor_border = '#dfa0f0',
  cursor_fg = '#fbf7f0',
  background = '#fbf7f0',
  foreground = '#000000',
  selection_bg = '#c2bcb5',
  selection_fg = '#000000',
}

config.enable_scroll_bar = false
config.term = "xterm-256color"

config.leader = { key = 'b', mods = 'CTRL', timeout_milliseconds = 1000 }
keys = {
  {
    key = '+',
    mods = 'CTRL',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = '-',
    mods = 'CTRL',
    action = wezterm.action.DisableDefaultAssignment,
  },
  -- I use this one in emacs. It is bound by default to fullscreen in wezterm
  {
    key = 'Enter',
    mods = 'META',
    action = wezterm.action.DisableDefaultAssignment,
  },
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
    key = '&',
    mods = 'LEADER',
    action = wezterm.action.CloseCurrentTab{ confirm=true },
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
     key = 'c',
     mods = 'CTRL',
     action = wezterm.action.ActivateKeyTable {
        name = 'emacs_like_ctr_c',
        one_shot = true,
     },
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
   -- simulate emacs eat ctrl-c ctrl-e and ctrl-c ctrl-e to switch to copy mode
   emacs_like_ctr_c = {
    {
       key = 'e',
       mods = 'CTRL',
       action = wezterm.action.ActivateCopyMode,
    },
    -- Cancel the mode by pressing escape
    { key = 'Escape', action = 'PopKeyTable' },
   },
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

local wayland_gnome = require 'wayland_gnome'
wayland_gnome.apply_to_config(config)

return config
