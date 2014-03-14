textadept.editing.STRIP_TRAILING_SPACES = false
ui.set_theme(not CURSES and 'dark' or 'termtheme', {font = 'Consolas', fontsize = 13})

textadept.file_types.extensions.spec = 'rpmspec'

trailing_ind = _SCINTILLA.next_indic_number()

-- mark trailing spaces
function highlight_trailing_spaces()
  --_G.print('----')
  --_G.print(buffer.filename)
  local top_line = buffer.first_visible_line
  local start_pos = buffer:position_from_line(top_line)
  local bottom_line = top_line + buffer.lines_on_screen + 1
  local end_pos = buffer:position_from_line(bottom_line)

  --_G.print('top_line:'..top_line.. ' bottom_line:'..bottom_line..' start_pos:'..start_pos..' end_pos:' ..end_pos)

  local text
  if (start_pos >= 0 and end_pos >= 0) and (end_pos >= start_pos) then
    text = buffer:text_range(start_pos, end_pos)
  else
    text = buffer:get_text()
    start_pos = 0
    end_pos = buffer.length
  end

  local saved_indicator_current = buffer.indicator_current
  buffer.indicator_current = trailing_ind
  buffer:indicator_clear_range(start_pos, end_pos - start_pos)
  for s, e in text:gmatch('()[ \t]+()\r?\n') do
    buffer:indicator_fill_range(start_pos + s - 1, e - (s - 1))
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


