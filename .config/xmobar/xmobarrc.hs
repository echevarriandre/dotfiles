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
            Run Date "%A, <fc=#ec77c1>%d</fc> %B <fc=#ec77c1>%Y</fc> (<fc=#ec77c1>%H</fc>:<fc=#ec77c1>%M</fc>:<fc=#ec77c1>%S</fc>)" "date" 10,
                
            -- Network up and down
            Run Network "enp0s31f6" [
                "-t", "<fn=1>\xf062</fn> <fc=#ec77c1><rx>kb</fc> <fn=1>\xf063</fn> <fc=#ec77c1><tx>kb</fc>"
            ] 20,

            -- Cpu usage in percent
            Run Cpu [
                "-t", "cpu: <fc=#ec77c1><total>% </fc>",
                "-H","50",
                "--high","red"
            ] 20,

            -- Ram used number and percent
            Run Memory [
                "-t", "mem: <fc=#ec77c1><used>M</fc> (<fc=#ec77c1><usedratio>%</fc>)"
            ] 20,

            -- Disk space free
            Run DiskU [("/", "hdd: <fc=#ec77c1><free></fc> free")] [] 60,

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
                "-t", "<fc=#ec77c1><fn=1><skyConditionS></fn> <tempC>°C</fc>"
            ] 36000,

            Run Alsa "default" "Master"
            [
                "-t", "<fn=1><status></fn> <fc=#ec77c1><volume>%</fc>",
                "--", "--on", "", "--off", "\xf6a9",
                "-h", "<fc=#dfdfdf>\xf028</fc>",
                "-m", "<fc=#dfdfdf>\xf027</fc>",
                "-l", "<fc=#dfdfdf>\xf026</fc>"
            ],

            -- Prints out the left side items such as workspaces, layout, etc.
            Run UnsafeStdinReader
        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "  %UnsafeStdinReader% } <fc=#dfdfdf> %date% </fc>  { <fc=#dfdfdf> %LPMR% </fc> <fc=#dfdfdf>%cpu% </fc><fc=#dfdfdf> %memory% </fc><fc=#dfdfdf> %disku% </fc><fc=#dfdfdf> %enp0s31f6% </fc><fc=#dfdfdf><fn=1></fn></fc> <fc=#ec77c1>%pacupdate%</fc> <fc=#dfdfdf>%alsa:default:Master%  </fc>"
        }
