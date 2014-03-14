textadept.editing.STRIP_TRAILING_SPACES = false
ui.set_theme(not CURSES and 'dark' or 'termtheme', {font = 'Consolas', fontsize = 13})

textadept.file_types.extensions.spec = 'rpmspec'

trailing_ind = _SCINTILLA.next_indic_number()

-- mark trailing spaces
function highlight_trailing_spaces()
  local text = buffer:get_text()

  -- TODO: only do it in the visible area, I haven't figured out yet

  --local start_pos = buffer:position_from_line(buffer.first_visible_line)
  --local end_pos = buffer.line_end_position[buffer.first_visible_line  + buffer.lines_on_screen + 1]
  --local text = buffer:text_range(start_pos, end_pos)

  local saved_indicator_current = buffer.indicator_current
  buffer.indicator_current = trailing_ind
  buffer:indicator_clear_range(0, buffer.length)
  for s, e in text:gmatch('()[ \t]+()\r?\n') do
    buffer:indicator_fill_range(s - 1, e - (s - 1))
  end
  buffer.indicator_current = saved_indicator_current
end

events.connect(events.UPDATE_UI, function()
  highlight_trailing_spaces()
end)

events.connect(events.BUFFER_NEW, function()
  buffer.indic_style[trailing_ind] = buffer.indic_style[textadept.editing.INDIC_HIGHLIGHT]
  buffer.indic_fore[trailing_ind] =  0x0000FF
end)

events.connect(events.BUFFER_AFTER_SWITCH, function()
  highlight_trailing_spaces()
end)


