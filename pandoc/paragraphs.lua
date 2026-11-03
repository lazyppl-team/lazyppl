-- Pandoc reads a paragraph that is followed directly, with no blank line, by a code block or an HTML block as Plain,
-- and the HTML writer then writes its text with no <p>. At the top level of the page, outside every raw HTML element,
-- this turns such a block into a paragraph. An image stays a block of its own, with the hard line break that follows it. A Plain block inside a raw element (the
-- text of a <summary>, the content of a <details> or a <div>) is left as the author wrote it.
local void = { area = true, base = true, br = true, col = true, embed = true, hr = true, img = true, input = true,
               link = true, meta = true, param = true, source = true, track = true, wbr = true }

local function depth_change(html)
  local d = 0
  for slash, name in html:gmatch('<(/?)(%a[%w]*)') do
    if not void[name:lower()] then d = d + (slash == '/' and -1 or 1) end
  end
  return d
end

local function is_blank(inline)
  return inline.t == 'Space' or inline.t == 'SoftBreak'
end

local function paragraph(run)
  while #run > 0 and is_blank(run[1]) do table.remove(run, 1) end
  while #run > 0 and is_blank(run[#run]) do table.remove(run) end
  if #run > 0 then return pandoc.Para(run) end
end

local function split_at_images(plain)
  local blocks, run = {}, {}
  local inlines = plain.content
  local i = 1
  while i <= #inlines do
    local inline = inlines[i]
    if inline.t == 'Image' then
      local p = paragraph(run)
      if p then blocks[#blocks + 1] = p end
      run = {}
      local image = { inline }
      if inlines[i + 1] and inlines[i + 1].t == 'LineBreak' then
        image[2] = inlines[i + 1]
        i = i + 1
      end
      blocks[#blocks + 1] = pandoc.Plain(image)
    else
      run[#run + 1] = inline
    end
    i = i + 1
  end
  local p = paragraph(run)
  if p then blocks[#blocks + 1] = p end
  return blocks
end

local function starts_a_block(block)
  return block ~= nil and (block.t == 'CodeBlock'
    or (block.t == 'RawBlock' and block.format == 'html' and block.text:match('^%s*<%a') ~= nil))
end

function Pandoc(doc)
  local out, depth = {}, 0
  for i, block in ipairs(doc.blocks) do
    if block.t == 'RawBlock' and block.format == 'html' then
      depth = depth + depth_change(block.text)
    end
    if depth == 0 and block.t == 'Plain' and starts_a_block(doc.blocks[i + 1]) then
      for _, b in ipairs(split_at_images(block)) do out[#out + 1] = b end
    else
      out[#out + 1] = block
    end
  end
  doc.blocks = out
  return doc
end
