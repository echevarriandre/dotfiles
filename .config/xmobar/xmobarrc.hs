-- http://projects.haskell.org/xmobar/

Config { 
        font = "xft:mononoki Nerd Font:weight=bold:pixelsize=13:antialias=true:hinting=false",
        additionalFonts = [
            "xft:Font Awesome 5 Free Solid:pixelsize=12"
        ],
        bgColor = "#000",
        fgColor = "#ff79c6",
        position = Top,
        lowerOnStart = True,
        hideOnStart = False,
        allDesktops = True,
        persistent = True,
        iconRoot = ".",  -- default: "."
        commands = [ 
            -- Time and date
            Run Date "%A, %d %B %Y (%H:%M:%S)" "date" 10,
                
            -- Network up and down
            Run Network "enp0s31f6" [
                "-t", "<fn=1>\xf0ab</fn> <rx>kb <fn=1>\xf0aa</fn> <tx>kb"
            ] 20,

            -- Cpu usage in percent
            Run Cpu [
                "-t", "<fn=1>\xf108</fn> cpu: (<total>%)",
                "-H","50",
                "--high","red"
            ] 20,

            -- Ram used number and percent
            Run Memory [
                "-t", "<fn=1>\xf233</fn> mem: <used>M (<usedratio>%)"
            ] 20,

            -- Disk space free
            Run DiskU [("/", "<fn=1>\xf0c7</fn> hdd: <free> free")] [] 60,

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
                "-t", "<fn=1><skyConditionS></fn> <tempC>°C"
            ] 36000,

            Run Alsa "default" "Master"
            [
                "-t", "<fn=1><status></fn> <volume>%",
                "--", "--on", "", "--off", "\xf6a9",
                "-h", "\xf028",
                "-m", "\xf027",
                "-l", "\xf026"
            ],

            -- Prints out the left side items such as workspaces, layout, etc.
            Run StdinReader
        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "  %StdinReader% } <fc=#8BE9FD> %date% </fc>  { <fc=#fee11a> %LPMR% </fc> <fc=#FFB86C>%cpu% </fc><fc=#FF5555> %memory% </fc><fc=#82AAFF> %disku% </fc><fc=#c3e88d> %enp0s31f6% </fc><fc=#e1acff><fn=1></fn> %pacupdate%</fc> <fc=#00ff00>%alsa:default:Master%  </fc>"
        }
