hs.hotkey.bind('cmd', 'f12', function()
    local kitty = hs.application.get('kitty')
    if not kitty or #kitty:allWindows() > 0 then
        hs.application.launchOrFocus('kitty')
    else
        kitty:selectMenuItem({'Shell', 'New OS Window'})
    end
end)
