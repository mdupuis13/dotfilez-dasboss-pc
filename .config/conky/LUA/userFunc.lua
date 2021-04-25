-- source: https://github.com/ajitjadhav28/conky-config

require 'cairo'

conky_start = 1
processor = ''
distribution = ''
mounted_media = ''
mounted_network_media = ''
--topprocesses = ''
cpus = -1
ncpu = 0
--cpu_temp_file = ''
--active_network_interface = false
--fan = -1
--fan_files = {}
--fan_rpms = {}
--ctemp = 0
start_flag = 1
--cpu_min_freq = {}
--cpu_max_freq = {}
swap = ''
--cpu_temp_update_freq = 4

-- Configs
--ENABLE_COLORS = true
--COLOR_SHUFFLE = 1

--_COLORS = {"#f1c40f", "#884ea0", "#3498db", "#2ecc71", "#ec7063", "#82e0aa", "#C41CB8", "#EDEA29"}
--COLORS = {"#ffffff"}

--_TOP_COLORS = {"#f3d346","#f4bb2f","#f3a422","#f18f1a","#f1721a","#f1441a"}
--_TOP_COLORS_SIZE = 6

--RED_START = 0x44
--RED_END = 0xff
--GREEN_START = 0xFF
--GREEN_END = 0x00

-- Conky main function
function conky_main()
    if conky_window == nil then
        return
    end
    local cs = cairo_xlib_surface_create(conky_window.display,
                                         conky_window.drawable,
                                         conky_window.visual,
                                         conky_window.width,
                                         conky_window.height)
    cr = cairo_create(cs)
    
    cairo_destroy(cr)
    cairo_surface_destroy(cs)
    cr=nil
    if start_flag == 1 then
        print("load number of cpus")
        local file = io.popen("lscpu -a -p='cpu' | tail -n 1")
        ncpu = trim(file:read("*a"))
        print(ncpu)
        file:close()

        print("load processor type info")
        local file = io.popen("lscpu | grep -Po '(?<=Model name:)(.*)'")
        processor = trim(file:read("*a"))
        print(processor)
        file:close()

        start_flag = 0
    end
end

-- Returns processor name
function conky_processor()
    return processor
end


-- Draws max n mounted partitions and its stats
function conky_mountmedia(n)
    if tonumber(conky_parse("$updates")) % 4 == 0 then
        local file = io.popen('df -h | grep -oE "/dev/[sd|nvme].*." | grep -oE "% (/.*)" | cut -d " " -f 2')
        local count = 1
        local media = ''
        local short_name = ''

        for line in file:lines() do
            if trim(line) == "/" then
                short_name = "root"
            else
                short_name = string.sub(string.sub(trim(line), string.find(trim(line), '[^/]*$')), 1)
            end
            
            if count <= tonumber(n) then
                media = media
                        .. "${template0 " .. short_name .. " " .. trim(line) .. " 20}\n"
            else
                break
            end
            count = count + 1
        end
        file:close()
        mounted_media = media
    end
    
    return mounted_media
end

-- Draws max n mounted partitions and its stats
function conky_mountdasnas(n)
    if tonumber(conky_parse("$updates")) % 4 == 0 then
        local file = io.popen('df -h | grep -oE "/mnt/dasnas/.*."')
        local count = 1
        local media = ''
        local short_name = ''

        for line in file:lines() do
            short_name = string.sub(string.sub(trim(line), string.find(trim(line), '[^/]*$')), 1)
            
            if count <= tonumber(n) then
                media = media
                        .. "${template0 " .. short_name .. " " .. trim(line) .. " 20}\n"
            else
                break
            end
            count = count + 1
        end
        file:close()
        mounted_network_media = media
    end 

    return mounted_network_media
end

-- Draws all cpu cores stats
function conky_drawcpus()
    if cpus == -1 or tonumber(conky_parse("$updates")) % 2 == 0 then
        local conky_cpus = ''

        for c = 1, (tonumber(ncpu)/2)+1  do
--            if c % 2 ~= 0 then
                conky_cpus = conky_cpus
                             .. "${template1 " .. c .. "}${template2 " .. c+4 .."}${color}\n"
--            end
        end

        cpus = conky_cpus
    end
    
    return cpus   
end

-- Shows swap if used mor than n percent
function conky_showswap(n)
    if tonumber(conky_parse("$updates")) % 4 == 0 then
    
        local swap_pct = tonumber(conky_parse("${swapperc}"))
        local conky_swap = ''

        if swap_pct >= tonumber(n) then
            conky_swap = "\nSWAP${goto 70}${voffset 2}$color3${swapbar 10,155}$color$alignr${voffset -2}$color${swap} / ${swapmax}\n"
        else
            conky_swap = nil
        end

        swap = conky_swap
    end

    return swap
end
--[[
-- Returns CPU temperature in Celsius
function conky_cputemp()
    if tonumber(conky_parse("$updates")) % cpu_temp_update_freq == 0 or ctemp == 0 then
        -- Get find cpu temp file on first call
        if ctemp == 0 then
            local all_hwmon_temp_names = io.popen('ls /sys/class/hwmon/*/temp* | grep -Po --regexp ".*(label)$"')
            for l in all_hwmon_temp_names:lines() do
                local name = io.popen('cat ' .. l):read("*a")
                if name:match("^Package*") then
                    cpu_temp_file = l:gsub("label", "input")
                    break
                end
            end
            all_hwmon_temp_names:close()
        end
        cpu_temp_file_handle = io.open(cpu_temp_file, "r")
        ctemp = tonumber(cpu_temp_file_handle:read("*a"))  / 1000
        cpu_temp_file_handle:close()
    end
    if ctemp > 75 then
        cpu_temp_update_freq = 2
        return "${color #ff0000}${font FontAwesome} ${font}${blink " .. ctemp .. "}${color}"
    elseif ctemp > 50 then
        cpu_temp_update_freq = 4
    else
        cpu_temp_update_freq = 6
    end
    return "${font FontAwesome} ${font}" .. ctemp
end

-- Returns Nth fan's speed in RPM
function conky_fanrpm(n)
    if tonumber(conky_parse("$updates")) % 4 == 0 or fan == -1 then
        if fan == -1 or fan_files[n] == nil then
            local all_hwmon_fans = io.popen('ls /sys/class/hwmon/*/fan?_input')
            for l in all_hwmon_fans:lines() do
                if l:match('fan' .. n .. '_input') then
                    fan_files[n] = l
                end
            end
            all_hwmon_fans:close()
        end
        local fan_file = io.open(fan_files[n], 'r')
        fan_rpms[n] = tonumber(fan_file:read('*a'))
        fan = 1
        fan_file:close()
        return fan_rpms[n]
    end
    return fan_rpms[n]
end

function get_color_freq_cpu(n)
    local min, max, cur, mid, mapped_color
    if cpu_min_freq[n+1] == nil then
        min = get_cpu_min_freq(n)
        if min == nil then
            min = 1200000
        end
        cpu_min_freq[n+1] = min
    else
        min = cpu_min_freq[n+1]
    end

    if cpu_max_freq[n+1] == nil then
        max = get_cpu_max_freq(n)
        if max == nil then
            max = 3000000
        end
        cpu_max_freq[n+1] = max
    else
        max = cpu_max_freq[n+1]
    end
    mid = math.floor(min+max/2)
    cur = get_cpu_freq(n)
    if cur == nil then
        cur = 2000000
    end
    if cur <= mid then
        red = map(cur, min, mid, RED_START, RED_END)
        mapped_color = string.format( "#%02xff00", red)
    else
        green = map(cur, mid, max, GREEN_START, GREEN_END)
        mapped_color = string.format( "#ff%02x00", green)
    end

    return mapped_color
end

function get_cpu_freq(cpu)
    local path = '/sys/devices/system/cpu/cpu' .. cpu .. '/cpufreq/scaling_cur_freq'
    local cpu_freq = io.open( path, 'r' )
    local freq = tonumber(cpu_freq:read('*a'))
    return freq
end

function get_cpu_max_freq(cpu)
    local path = '/sys/devices/system/cpu/cpu' .. cpu .. '/cpufreq/cpuinfo_max_freq'
    local cpu_freq = io.open( path, 'r' )
    local freq = tonumber(cpu_freq:read('*a'))
    return freq
end

function get_cpu_min_freq(cpu)
    local path = "/sys/devices/system/cpu/cpu" .. cpu .. "/cpufreq/cpuinfo_min_freq"
    local cpu_freq = io.open( path, 'r' )
    local freq = tonumber(cpu_freq:read('*a'))
    return freq
end

function conky_topprocess(n)
    local p
    col = "${color #ffffff}"
    if topprocesses == '' then
        topprocesses = topprocesses .. "${color #00FF00}${goto 10}Name ${goto 190}Pid${goto 255}Cpu%${goto 310}Mem%${color}\n"
        for p = 1, tonumber(n) do
            if ENABLE_COLORS then
                col = "${color " .. _TOP_COLORS[(_TOP_COLORS_SIZE + 1) - p] .. "}"
            end
            topprocesses = topprocesses
                        .. col .. "${goto 10}${top name ".. p .. "} ${goto 180}${top pid ".. p .. "}${goto 235}${top cpu ".. p
                        .. "}${goto 290}${top mem ".. p .. "}${color}\n"
        end   
    end
    return topprocesses
end
]]

function map(x, in_min, in_max, out_min, out_max)
  return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min
end

-- Trims given string and returns
function trim(s)
   return s:gsub("^%s+", ""):gsub("%s+$", "")
end

-- shuffle function
function shuffle(tbl)
  for i = #tbl, 2, -1 do
    local j = math.random(i)
    tbl[i], tbl[j] = tbl[j], tbl[i]
  end
  return tbl
end
