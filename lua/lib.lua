--------------------------------------------------------
-- Utility functions.                                 --
--------------------------------------------------------
ms = {}

-- Generates a random integer for an arbitrary table.
-- @param t table
function ms.table_to_int(t)
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
