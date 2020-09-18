-- http://projects.haskell.org/xmobar/

Config { 
        font = "xft:mononoki Nerd Font:weight=bold:pixelsize=13:antialias=true:hinting=false",
        additionalFonts = [
            "xft:Font Awesome 5 Free Solid:pixelsize=12"
        ],
        bgColor = "#000000", -- // #282a36
        fgColor = "#44475a",
        position = Top,
        lowerOnStart = True,
        hideOnStart = False,
        allDesktops = True,
        persistent = True,
        iconRoot = ".",  -- default: "."
        commands = [ 
            -- Time and date
            Run Date "%A, <fc=#ff79c6>%d</fc> %B <fc=#ff79c6>%Y</fc> (<fc=#ff79c6>%H</fc>:<fc=#ff79c6>%M</fc>:<fc=#ff79c6>%S</fc>)" "date" 10,
                
            -- Network up and down
            Run Network "enp0s31f6" [
                "-t", "<fn=1>\xf062</fn> <fc=#ff79c6><rx>kb</fc> <fn=1>\xf063</fn> <fc=#ff79c6><tx>kb</fc>"
            ] 20,

            -- Cpu usage in percent
            Run Cpu [
                "-t", "cpu: <fc=#ff79c6><total>% </fc>",
                "-H","50",
                "--high","red"
            ] 20,

            -- Ram used number and percent
            Run Memory [
                "-t", "mem: <fc=#ff79c6><used>M</fc> (<fc=#ff79c6><usedratio>%</fc>)"
            ] 20,

            -- Disk space free
            Run DiskU [("/", "hdd: <fc=#ff79c6><free></fc> free")] [] 60,

            -- Runs custom script to check for pacman updates.
            -- This script is in my dotfiles repo in .local/bin.
            Run Com "pacupdate" [] "" 3000,

            -- Runs a standard shell command 'uname -r' to get kernel version
            Run Com "uname" ["-r"] "" 3600,

            -- Weather
            Run WeatherX "LPMR"
            [
                ("clear", "\xf185"),
                ("sunny", "\xf185"),
                ("mostly clear", "\xf6c4"),
                ("mostly sunny", "\xf6c4"),
                ("partly sunny", "\xf6c4"),
                ("fair", "\xf185"),
                ("cloudy","\xf0c2"),
                ("overcast","\xf0c2"),
                ("partly cloudy", "\xf6c4"),
                ("mostly cloudy", "\xf73d"),
                ("considerable cloudiness", "\xf740")
            ]
            [
                "-t", "<fc=#ff79c6><fn=1><skyConditionS></fn> <tempC>°C</fc>"
            ] 36000,

            Run Alsa "default" "Master"
            [
                "-t", "<fn=1><status></fn> <fc=#ff79c6><volume>%</fc>",
                "--", "--on", "", "--off", "\xf6a9",
                "-h", "<fc=#f8f8f2>\xf028</fc>",
                "-m", "<fc=#f8f8f2>\xf027</fc>",
                "-l", "<fc=#f8f8f2>\xf026</fc>"
            ],

            -- Prints out the left side items such as workspaces, layout, etc.
            Run UnsafeStdinReader
        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "  %UnsafeStdinReader% } <fc=#f8f8f2> %date% </fc>  { <fc=#f8f8f2> %LPMR% </fc> <fc=#f8f8f2>%cpu% </fc><fc=#f8f8f2> %memory% </fc><fc=#f8f8f2> %disku% </fc><fc=#f8f8f2> %enp0s31f6% </fc><fc=#f8f8f2><fn=1></fn></fc> <fc=#ff79c6>%pacupdate%</fc> <fc=#f8f8f2>%alsa:default:Master%  </fc>"
        }
