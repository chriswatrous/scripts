#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

PlaceActiveWindow(mx, my, mw, mh)
{
    SetWinDelay 10
    SysGet mons, MonitorCount
    if (mons == 1)
    {
        SysGet, mon, MonitorWorkArea, 1
    }
    else if (mons == 2)
    {
        SysGet, mon1, MonitorWorkArea, 1
        SysGet, mon2, MonitorWorkArea, 2
        flip := mon1left > mon2left
        WinGetPos x, y, w, h, A
        xlimit := (flip ? mon1left : mon2left) - w/2
        monnumber := (x < xlimit) != flip ? 1 : 2
        SysGet, mon, MonitorWorkArea, %monnumber%
    }
    monwidth := monright - monleft
    monheight := monbottom - montop
    WinRestore A
    WinMove A, , monleft + mx*monwidth, montop + my*monheight, mw*monwidth, mh*monheight`
}
