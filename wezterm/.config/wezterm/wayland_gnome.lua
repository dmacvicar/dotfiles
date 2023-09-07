local wezterm = require 'wezterm'
local mod = {}

local function gsettings(key)
  return wezterm.run_child_process({"gsettings", "get", "org.gnome.desktop.interface", key})
end

function mod.apply_to_config(config)
  if wezterm.target_triple ~= "x86_64-unknown-linux-gnu" then
    -- skip if not running on linux
    return
  end
  local success, stdout, stderr = gsettings("cursor-theme")
  if success then
    config.xcursor_theme = stdout:gsub("'(.+)'\n", "%1")
  end

  local success, stdout, stderr = gsettings("cursor-size")
  if success then
    config.xcursor_size = tonumber(stdout)
  end

  local success, stdout, stderr = gsettings("monospace-font-name")
  if success then
     sz, _ = stdout:gsub("'(.+) (%d+)'\n", "%2")
     config.font_size = tonumber(sz)
     config.font = wezterm.font_with_fallback {
        stdout:gsub("'(.+) (%d+)'\n", "%1"),
        'Source Code Pro',
     }
  end

  config.enable_wayland = true

  if config.enable_wayland and os.getenv("WAYLAND_DISPLAY") then
    local success, stdout, stderr = gsettings("text-scaling-factor")
    if success then
      config.font_size = (config.font_size or 10.0) * tonumber(stdout)
    end
  end

end

return mod
