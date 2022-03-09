-- Simplified version of TJs because im kinda dumb and
-- need to make it easy for myself. Implemented as an
-- exercise in Lua.

-- Closure database.
-- =================
__KEY_MAP_CLOSURES = {}

-- Table of mappings.
__KEY_MAP_CLOSURES.closures = {}

-- Method to call a loaded closure.
function __KEY_MAP_CLOSURES.call(id)
    __KEY_MAP_CLOSURES.closures[id]()
end


-- Key remapping and closure database management.
-- ==============================================

local load_closure = function(fn)
    -- Insert `fn` into the table of closures.
    table.insert(__KEY_MAP_CLOSURES.closures, fn)

    -- The functions id is the following length of the table.
    return #__KEY_MAP_CLOSURES.closures
end

local remap = function(mode, defaults, opts)
    -- Options passed as key-value pairs is interpreted
    -- as parameters; separate them out.
    local mapping, params = {}, {}
    for k, v in pairs(opts) do
        if type(k) ==  'number' then
            mapping[k] = v
        else
            params[k] = v
        end
    end

    -- A typical mapping at this point is
    --   mapping = { 'g[', <lua closure> }
    --   params  = { silent = true  }
    -- and mapping should generally have two elements.
    lhs, rhs = mapping[1], mapping[2]

    -- The key combination is always a string (vim key syntax)
    if type(lhs) ~= 'string' then
        error("LHS needs to be a string")
    end

    -- The rhs can be vimscript code or a lua closure.
    -- In the event it is a closure, this needs to be loaded
    -- into the database, and the command is code that calls
    -- it.
    if type(rhs) == 'string' then
        command = rhs
    elseif type(rhs) == 'function' then
        fun = load_closure(rhs)
        command = string.format(
            [[<cmd>lua __KEY_MAP_CLOSURES.call(%s)<CR>]], fun
        )
    else
        error("Can't map " .. tostring(rhs))
    end

    -- Extend the defaults with the users provided options.
    opts = vim.tbl_extend("force", defaults, params)

    -- Register the key remapping.
    vim.api.nvim_set_keymap(mode, lhs, command, opts)
end


-- Actual key remapping API.
-- =========================

function nmap(mapping)
    remap('n', { }, mapping)
end

function nnoremap(mapping)
    remap('n', { noremap = true }, mapping)
end

function vmap(mapping)
    remap('v', { }, mapping)
end

function vnoremap(mapping)
    remap('v', { noremap = true }, mapping)
end

function imap(mapping)
    remap('i', { }, mapping)
end

function inoremap(mapping)
    remap('i', { noremap = true }, mapping)
end

function colorscheme(name)
    vim.cmd(string.format('colorscheme %s', name))
end
