function showKitty()
   local kitty = hs.application.get('kitty')
   if not kitty or #kitty:allWindows() > 0 then
      hs.application.launchOrFocus('kitty')
   else
      kitty:selectMenuItem({'Shell', 'New OS Window'})
   end
end

hs.hotkey.bind('cmd', 'f12', showKitty)


function focusedWindowLayout(unit)
   return function()
      hs.layout.apply({{nil, hs.window.focusedWindow(), nil, unit, nil, nil}})
   end
end

hs.hotkey.bind('cmd', 'left', focusedWindowLayout(hs.layout.left50))
hs.hotkey.bind('cmd', 'right', focusedWindowLayout(hs.layout.right50))
hs.hotkey.bind('cmd', 'up', focusedWindowLayout(hs.layout.maximized))
hs.hotkey.bind('cmd', 'down', function() end)
