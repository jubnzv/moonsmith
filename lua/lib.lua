-- Functions used by random code generated with moonsmith.
-- Using of this module is optional and could be disabled in the runtime.

local M = {}

-- Generates a random integer for an arbitrary table.
function M.table_to_int(t)
  acc = 0
  for i, v in ipairs(t) do
    if type(v) == "number" then
      acc = acc + v
    else
      acc = acc & i
    end
  end
  return acc + #t
end

return M
