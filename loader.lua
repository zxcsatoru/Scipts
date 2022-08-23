--Cheat.load_script("Dead-Sync_4.lua")
--Cheat.unload_script("loader.lua")
function render()
    local height = Engine.get_screen_height()
    local width = Engine.get_screen_width()
    Draw.RenderRectFilled(width, height, 0, 0, Color.new(20, 20, 20, 100))
    Draw.RenderRectFilled(200, 200, 10, 10, Color.new(20, 20, 20, 250))
end

Cheat.add_callback("Render", render)