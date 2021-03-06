#NoEnv
#Persistent
if FileExist("C:/Users/Chris") != ""
{
    Home = 1
    WordProcessor = C:\Program Files (x86)\LibreOffice 4\program\swriter.exe
    Spreadsheet = C:\Program Files (x86)\LibreOffice 4\program\scalc.exe
    VisualStudio = C:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\IDE\devenv.exe
    VisualStudioProjects = Explorer C:\Users\chris\Documents\Visual Studio 2012\Projects
}
if FileExist("C:/Users/IBM_ADMIN") != ""
{
    Work = 1
    WordProcessor = C:\Program Files (x86)\OpenOffice 4\program\swriter.exe
    Spreadsheet = C:\Program Files (x86)\OpenOffice 4\program\scalc.exe
}
Return

^Capslock::Capslock
Capslock::LCtrl

!#a::Run "C:\Program Files (x86)\Vim\vim74\gvim.exe" %A_MyDocuments%/AutoHotkey.ahk
^#a::Run AutoHotkey

;#a::Run 
#c::Run Chrome
#g::Run C:\Program Files (x86)\Google\Google Earth Pro\client\googleearth.exe
#h::Run C:\cygwin64\bin\mintty.exe -w max -i /Cygwin-Terminal.ico -
#+h::Run %A_MyDocuments%/../Program Files/Cygwin-setup-x86_64.exe
#j::RemoteLogin1()
#n::Run Notepad
#o::StartOctave()
#q::Run Explorer %A_MyDocuments%\stuff
#s::Run acrord32
#v::Run C:\Program Files (x86)\JetBrains\PyCharm Community Edition 4.5.1\bin\pycharm.exe
#w::Run %WordProcessor%
#x::Run %Spreadsheet%
#y::Run powershell
#^y::Run cmd
#+y::Run "C:\Program Files (x86)\Vim\vim74\gvim.exe" C:\Users\Chris\Documents\WindowsPowerShell\profile.ps1
#z::Run Firefox
#]::WinMinimize A

;I don't use this shortcut much and it can get anoying.
;+Space::_



; Unused Keys Win+Key combinations
;#Escape
;#k
;#`
;#,
;#i
;#.
;#;
;#/
;#[
;#'
;#]
;#\
;#Backspace
;#Enter
#Insert::ClipText()
;#Delete
;#Home
;#End
;#PageDown
;#PageUp


; Don't use these key combinations
;#e opens explorer
;#d shows desktop
;#r opens run dialog
;#f opens windows search
;#t focuses task bar
;#b focuses notification area
;#u opens ease of use control panel
;#m opens start menu along with program
;#l locks workstation when regular windows key is used
;#p opens some kind of screen management menu
;win + top row numberkeys, selects programs from the task bar
;#- windows magnifier zoom out
;#= windows magnifier zoom in
;#Space
;win + arrow keys, manipulates windows

#Numpad0::PlaceActiveWindow(0, 0, 1, 1)

#Numpad1::PlacePattern1()
PlacePattern1()
{
    global LastPattern
    global Spot
    if (LastPattern != 1)
        Spot := -1
    LastPattern := 1
    Spot := mod(Spot + 1, 2)
    if (Spot == 0)
        PlaceActiveWindow(0, 0, 1, 1/2)
    if (Spot == 1)
        PlaceActiveWindow(0, 1/2, 1, 1/2)
}

#Numpad2::PlacePattern2()
PlacePattern2()
{
    global LastPattern
    global Spot
    if (LastPattern != 2)
        Spot := -1
    LastPattern := 2
    Spot := mod(Spot + 1, 2)
    if (Spot == 0)
        PlaceActiveWindow(0, 0, 1/2, 1)
    if (Spot == 1)
        PlaceActiveWindow(1/2, 0, 1/2, 1)
}

#Numpad3::PlacePattern3()
PlacePattern3()
{
    global LastPattern
    global Spot
    if (LastPattern != 3)
        Spot := -1
    LastPattern := 3
    Spot := mod(Spot + 1, 4)
    if (Spot == 0)
        PlaceActiveWindow(0, 0, 1/2, 1/2)
    if (Spot == 1)
        PlaceActiveWindow(1/2, 0, 1/2, 1/2)
    if (Spot == 2)
        PlaceActiveWindow(0, 1/2, 1/2, 1/2)
    if (Spot == 3)
        PlaceActiveWindow(1/2, 1/2, 1/2, 1/2)
}

#Numpad4::PlacePattern4()
PlacePattern4()
{
    global LastPattern
    global Spot
    if (LastPattern != 4)
        Spot := -1
    LastPattern := 4
    Spot := mod(Spot + 1, 2)
    if (Spot == 0)
        PlaceActiveWindow(0, 0, 1, 2/3)
    if (Spot == 1)
        PlaceActiveWindow(0, 2/3, 1, 1/3)
}

#Numpad5::PlacePattern5()
PlacePattern5()
{
    global LastPattern
    global Spot
    if (LastPattern != 5)
        Spot := -1
    LastPattern := 5
    Spot := mod(Spot + 1, 2)
    if (Spot == 0)
        PlaceActiveWindow(0, 0, 1, 3/4)
    if (Spot == 1)
        PlaceActiveWindow(0, 3/4, 1, 1/4)
}


#`::ToggleWindowBorder()
#\::ToggleWindowBorder()
ToggleWindowBorder()
{
    SetWinDelay 10
    WinGet state, MinMax, A
    if (state == 1)
        PlaceActiveWindow(0, 0, 1, 1)
    WinSet Style, ^0x840000, A
    WinGetPos X, Y, Width, Height, A
    WinMove A, , X, Y, Width, Height + 1
    WinMove A, , X, Y, Width, Height
}

;#^f::ToggleFocusFollow()
;FocusFollow = 0
;ToggleFocusFollow()
;{
;    global FocusFollow
;    FocusFollow := !FocusFollow
;    SetFocusFollow(FocusFollow)
;}
;SetFocusFollow(enabled)
;{
;    SPI_SETACTIVEWINDOWTRACKING = 0x1001
;    SPIF_UPDATEINIFILE = 1
;    SPIF_SENDCHANGE = 2
;    DllCall("SystemParametersInfo",UInt,SPI_SETACTIVEWINDOWTRACKING,UInt,0,UInt,enabled,UInt,SPIF_UPDATEINIFILE | SPIF_SENDCHANGE)
;}

RemoteLogin1()
{
    global Home, Work
    if Home
    {
        Run C:\cygwin64\bin\mintty.exe -w max /usr/bin/ssh chris@192.168.1.50
    }
    if Work
    {
        Run C:\cygwin64\bin\mintty.exe -w max /usr/bin/ssh chris@192.168.56.101
    }
}

WorkLogin(server)
{
    SetWinDelay 10
    Run C:\cygwin64\bin\mintty.exe -w max -i /Cygwin-Terminal.ico --title mintty /usr/bin/ssh cwatrous@%server%
    WinWait mintty
    SendInput . .bashrc{Enter}
}


StartOctave()
{
    Run C:\cygwin64\bin\mintty.exe --title mintty -w max -i /Cygwin-Terminal.ico -
    WinWait mintty
    SendInput octave -q{Enter}
}

ClipText()
{
    clipboard = %clipboard%
}
