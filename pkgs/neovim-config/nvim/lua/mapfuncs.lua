local function cd ()
	vim.api.nvim_command("lcd " .. vim.fn.expand("%:h"))
end

local function z ()
  print("Hello from z.")
end

return {
	cd = cd,
	z = z
}
