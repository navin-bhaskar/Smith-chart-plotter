
                           ' shree krishnaya namaha '

'   Redistribution and use in source and binary forms, with or without
'   modification, are permitted provided that the following conditions are
'   met:
'
'   * Redistributions of source code must retain the above copyright
'     notice, this list of conditions and the following disclaimer.
'   * Redistributions in binary form must reproduce the above
'     copyright notice, this list of conditions and the following disclaimer
'     in the documentation and/or other materials provided with the
'     distribution.
'   * Neither the name of the  nor the names of its
'     contributors may be used to endorse or promote products derived from
'     this software without specific prior written permission.
'
'   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
'   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
'   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
'   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
'   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
'   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
'   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
'   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
'   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
'   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
'   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


' Name:Smith Chart Plotter
' Author:Navin Bhaskar
' Description:Program thet plots circles
' In this program i've used (650,320) as origin

   global scale
   scale=300                                           ' This is our scaling factor all the plots are scaled to
    nomainwin                                          ' this number, change it if you want a different scaling factor


    upperLeftX  = 50
    upperLeftY= 10
    TextboxColor$ = "cyan"
    ForegroundColor$ = "red"
    WindowWidth = 1020
    WindowHeight = 760
'------------------------------------------------------------------------------------------------------------------------------------
'    windows creation section                                                                                  '
'------------------------------------------------------------------------------------------------------------------------------------
    graphicbox #main.graph, 6, 41, 700, 640
    textbox #main.textbox2, 262, 11, 72, 25
    statictext #main.statictext3, "Enter Here Charecteristic impedence (zo)", 6, 16, 248, 20
    statictext #main.statictext4, "Enter Here the r value", 382, 16, 136, 25
    textbox #main.textbox5, 526, 11, 72, 25
    statictext #main.statictext6, "ohm", 606, 16, 40, 20
    statictext #main.statictext7, "Enter here x Value", 678, 16, 120, 20
    textbox #main.textbox8, 814, 11, 72, 25
    statictext #main.statictext10, "ohm", 894, 16, 32, 20
    button #main.button11, "Plot", [button11Click], UL, 780, 96, 160, 25
    button #main.button14, "About", [button14Click], UL, 827, 561, 50, 25
    button #main.quit, "Quit", [Quit], UL, 795, 606, 120, 25
    statictext #main.statictext15, "ohm", 342, 16, 32, 20
    button #main.button21, "Take a Snap Shot", [snap], UL, 782, 471, 144, 25
    button #main.button19, "New", [new], UL, 782, 520, 144, 25
    TextboxColor$ = "White"
    ForegroundColor$ = "dark cyan"
    statictext #main.statictext16, "Impedance of the network is:", 766, 161, 176, 20
    textbox #main.znot, 774, 251, 152, 25
    statictext #main.statictext18, "Characteristic impedance is:", 774, 226, 168, 20
    statictext #main.statictext23, "Normalized impedance is:", 774, 291, 168, 20
    textbox #main.norm, 774, 316, 152, 25
    statictext #main.statictext25, "VSWR is", 774, 351, 144, 20
    statictext #main.statictext28, "ohm", 929, 256, 144, 20
    statictext #main.statictext30, "ohm", 929, 191, 144, 20
    statictext #main.admittance, "Admittence of the network is:", 766, 411, 176, 20
    statictext #main.statictext31, "mho", 929, 438, 144, 20
    textbox #main.vswr, 774, 371, 152, 25
    textbox #main.imp, 774, 186, 152, 25
    textbox #main.amitnce, 774, 431, 152, 25
    open " Smith Chart plotter v5" for window as #main
    print #main.textbox2, "!setfocus"
    print #main.imp,"!disable"
    print #main.norm,"!disable"
    print #main.amitnce,"!disable"
    print #main.vswr,"!disable"
    print #main.znot,"!disable"
    print #main.graph, "fill white; flush"
    print #main, "font ms_sans_serif 0 16"
    print#main.graph, "flush"
    print#main, "trapclose [Quit]"
'---------------------------------------------------------------------------------------------------------------------------------------
'                         end of the section
'---------------------------------------------------------------------------------------------------------------------------------------

[main.inputLoop]
    cn = 0  'wait here for input event
    wait
'***************************************************************************************************************************************'
'                        This section is for smith chart calculations                                                                      '
'***************************************************************************************************************************************'

[button11Click]
' so this is the main section which is going to plot circles for us
    cn = cn+1
' this is done so that graphs are not drawn twice
    if cn >= 2 then  wait
    print #main.textbox2, "!contents? z0"
    print #main.textbox5, "!contents? r"
    print #main.textbox8, "!contents? x"
' this error handling is important else you will end up dividing a number by 0
    if z0 = 0 then notice "characteristic impedance can't be zero or you have not entered characteristic impedence": goto[main.inputLoop]
    if r = 0 then notice "You have not entered r": goto[main.inputLoop]
    if x = 0 then notice "You have not entered X": goto[main.inputLoop]
    normr = r/z0
    normx = x/z0


rem here we have normalized r and x

    kreal = (1 - (normr*normr) - (normx*normx))/((1- (normr))*(1- (normr))+ normx*normx)
    kimag = 2*normx/((1- (normr))*(1- (normr))+ normx*normx)
    k = sqr(kreal*kreal+ kimag*kimag)
    theta = atn(kreal/kimag)*180/3.141592
    vswr = (1+k)/(1-k)
    r1 = normr
    x1 = normx

    if r1 < 0 then notice "The system you are trying to describe to me dosen't seem to exist physically, it reflects greater energy back than the incident energy which obviously breaks basic physical laws ": goto[main.inputLoop]
    ' phew that was the longest line in this whole code source file
    if x > 0 then
        print #main.imp, using("####.###",r); " +j ";using("####.###", x)
        print #main.norm, using("####.###",normr); " +j ";  using("####.###",normx)
        if  x1/(r1*r1+x1*x1) > 0 then
        print #main.amitnce,  using("####.###",r1/(r1*r1+x1*x1)); " +j ";  using("####.###",x1/(r1*r1+x1*x1))
        else 'if (x1/(r1*r1-x1*x1)) < 0 then
        print #main.amitnce,  using("####.###",r1/(r1*r1+x1*x1)); " -j ";  using("####.###",abs(x1/(r1*r1+x1*x1)))
        end if
    else
        print #main.imp,  using("####.###",r); " -j ";  using("####.###",abs(x))
        print #main.norm,  using("####.###",normr); " -j ";  using("####.###",abs(normx))
        if x1/(r1*r1+x1*x1) > 0 then
        print #main.amitnce,  using("####.###",r1/(r1*r1+x1*x1)); "  -j ";  using("####.###",abs(x1/(r1*r1+x1*x1)))
        else 'if x1/(r1*r1-x1*x1) < 0 then
        print #main.amitnce,  using("####.###",r1/(r1*r1+x1*x1)); " +j ";  using("####.###",x1/(r1*r1+x1*x1))
        end if
    end if
    print #main.vswr, vswr
    print #main.znot, z0
    call drawGraph
    print #main.button21, "!disable"
    print #main.button19, "!disable"
    rl = normr
    let posiX = (rl/(rl+1)) *scale
    let posR = (1.0/(rl+1))*scale
    for i=0 to 2
    print#main.graph, "up; down;north ; turn 90"
    if i=0 then selColor$ = "color 22 150 90"
    if i=1 then selColor$ = "color 150 200 180"
    if i=2 then selColor$ = "color red"
    print#main.graph, selColor$
    print#main.graph, "up; goto 350 320; go "; (posiX); " up; down; circle "; posR; " north ; up ; turn 225; go "; posR+15;
    print#main.graph, "\"; rl
    timer 500, [finR]
    wait
    [finR]
    timer 0
    next i
    for i=0 to 2
    print#main.graph, "north"
    if i=0 then selColor$ = "color 22 150 90"
    if i=1 then selColor$ = "color 150 200 180"
    if i=2 then selColor$ = "color red"
    print#main.graph, selColor$
    if normx > 0 then
        rl = normx
        let posiX1 = (1/rl) *scale
        let posR1 = (1/rl)*scale
        print#main.graph, "up; goto 650 320; go "; (posiX1); " up; down; circle "; posR1; " north ; up ; turn 270; go "; posR1+15;
        print#main.graph, "\"; rl
    else
        rl = abs(normx)
        let posiX2 = (1/rl) *scale
        let posR2 = (1/rl)*scale
        print#main.graph, "up;turn 180; goto 650 320; go "; (posiX2); " up; down; circle "; posR2 ; "up; up ; turn 90; go "; posR2+15;
        print#main.graph, "\"; rl
    end if
    timer 500, [finX]
    wait
    [finX]
    timer 0
    next i
    swr = (1+vswr)/(1-vswr)

    print#main.graph, "north"
    print#main.graph, "color cyan"
    print#main.graph, "backcolor 085 214 255;size 3"
    print#main.graph, "up; goto 10 17;  down "
    print#main.graph, "down; boxfilled 127 100"
    print#main.graph, "color 172 035 070"
    print#main.graph, "up; goto 15 35; size 10; down "
    print#main.graph, "\Z0="; z0
    print#main.graph, "up; goto 15 51.5; size 10; down "
    print#main.graph, "\R="; normr
    print#main.graph, "up; goto 15 68; size 10; down "
    print#main.graph, "\X="; normx
    print#main.graph, "\Angle=";theta
    print#main.graph, "color 255 000 115; size 3"

    dist = abs(swr*scale)
    print#main.graph, "up; goto 350 320 ; down; circle "; dist
    print#main.graph, "color blue; size 5"
    if x>0 then
    print#main.graph, "up; goto 350 320; turn "; 360-theta;" down; down; go "; dist
    print#main.graph, "up; goto 350 320; turn 180;down; go ";dist
    else
    print#main.graph, "up; goto 350 320; turn "; 180-theta;" down; down; go "; dist
    print#main.graph, "up; goto 350 320; turn 180;down; go ";dist
    end if

    print#main.graph, "flush"
    flag = 1
    print #main.button21, "!enable"
    print #main.button19, "!enable"
    wait
'-------------------------------------------------------------------------------------------------------------------------------------
'                       About box section
'-------------------------------------------------------------------------------------------------------------------------------------
' About modal dialog box

[button14Click]
    UpperLeftX = 200
    UpperLeftY = 90
    ForegroundColor$ = "brown"
    WindowWidth = 362
    WindowHeight = 330
    statictext #main1.abttxt, "This software is dedicated to my parents, " , 5, 5, 400, 18
    statictext #main1.abttxt1, "Bhaskar and Jayalaxmi" , 5, 20, 200, 20
    statictext #main1.abttxt2, "my teachers, my brother Nitin and my dearest friend  ", 5, 37, 400, 32
    statictext #main1.abttxt3, "Prasnna",5,53,100,22
    graphicbox #main1.graph, 22, 80, 306, 183
    bmpbutton #main1.bmpbutton1, "bmp\btn.bmp", [button4Click], UL, 125, 225
    button #main1.quit, "Ok", [quit1], UL, 124, 270, 81, 22
    open "About " for dialog_modal as #main1
    print #main1.graph,"down; fill white; flush"
    print #main1, "trapclose [quit.main]"
    print #main1, "font ms_sans_serif 10"
    print #main1.graph, "font WST_Fren 0 15"
    loadbmp "navLogo", "bmp\logo.bmp"
    print#main1.graph, "drawbmp navLogo 0 0"
    print#main1.graph, "backcolor 244 215 215"
    print #main1.graph, "Place 25 35"
    print#main1.graph, "\(c) Navin Bhaskar "
    print#main1.graph, "\Programmed using just basic, this software is "
    print #main1.graph, "\Distrubuted under AS Is condition"
    print#main1.graph, "\Mini Help: Just enter the values of "
    print#main1.graph, "\characteristic impedeance and r and x in their  "
    print #main1.graph, "\respective fields and press 'plot'"
    print#main1.graph, "\r is for real part and x for imaginary part"
    print#main1.graph, "\for more info, "
    print#main1.graph, "font WST_Fren 0 15"
    print#main1.graph, "flush"
    print #main1 , "trapclose [quit1]"
    wait

[button4Click]
 'run "C:\Program Files\Internet Explorer\iexplore.exe " +  DefaultDir$ + "\help.htm"
  run "explorer " +  DefaultDir$ + "\help.htm"
[quit1]
    close  #main1
    unloadbmp "navLogo"
    wait

[jump]
    wait
'---------------------------------------------------------------------------------------------------------------------------------
'                              end of about box section
'---------------------------------------------------------------------------------------------------------------------------------
'-------------------------------------------------------
'    New button
'-------------------------------------------------------

[new]
    cn = 0
'counter set to zero
    print #main.graph, "cls"
    print #main.graph, "backcolor 255 255 255"
    print #main.imp, ""
    print #main.vswr, ""
    print #main.znot, ""
    print #main.norm, ""
    print #main.textbox2, ""
    print #main.textbox5, ""
    print #main.textbox8, ""
    print #main.amitnce, ""
    flag = 0
    print#main.graph, " size 1; north"
    print #main.textbox2, "!setfocus"
    wait
'----------------------------------------------------------------------------------------------------------------------------------
'                         this sub is to draw smith chart
'----------------------------------------------------------------------------------------------------------------------------------

sub drawGraph
' well we are going to start with the r-circles
    print#main.graph, "cls"
    print#main.graph, "color 235 167 50"
    print#main.graph, "home; down; turn 90; go 380;home; turn 180; go 380; up; home"
    print#main.graph, " goto 300 300; down; turn 180"
    for i = 7 to 0 step -.25
        rl = i
        let posiX = (rl/(rl+1)) *scale
        let posR = (1.0/(rl+1))*scale
        print#main.graph, "up; goto 350 320; go "; (posiX); " up; down; circle "; posR
    next i
    print#main.graph, "up; goto 350 320; down;turn 90; go 320; goto 350 320; turn 180; go 350; up; turn 90"
    print#main.graph, "up;  goto 650 320; down; turn 90; turn 180"
    for i = 7 to .25 step -.25
        rl = i
        let posiX = (1/rl) *scale
        let posR = (1/rl)*scale
        print#main.graph, "up; goto 650 320; go "; (posiX); " up; down; circle "; posR
    next i
    print#main.graph, "up;  goto 650 320; down; turn 180 "
    for i = 7 to .25 step -.25
        rl = i
        let posiX = (1/rl) *scale
        let posR = (1/rl)*scale
        print#main.graph, "up; goto 650 320; go "; (posiX); " up; down; circle "; posR
    next i
    print#main.graph, "color 100 100 200 "
    print#main.graph, "turn 585"
    for i= 360 to 45 step -45
        select case i
            case 45
            x = 310
            case 90                     'these cases are for proper indention
            x = 308
            case 180
            x = 330
            case 225
            x = 330
            case 270
            x = 316
            case 360
            x = 304
        case else
            x = 320
        end select
        print#main.graph, "up; goto 350 320; turn 45; go "; x
        print#main.graph, "\ "; i
    next i
    print#main.graph, "color 150 70 50"
    print#main.graph, "goto 15 15; down "
    print#main.graph, "\POSITIVE REACTANCE COMPONENT"
    print#main.graph, "up"
    print#main.graph, "up; UP; goto 15 635; down "
    print#main.graph, "\NEGATIVE REACTANCE COMPONENT"
    print#main.graph, "up"

    print#main.graph, "goto 590, 615"
    print#main.graph, "font WST_Fren 0 15"
    print#main.graph, "color PaleGray"
    print#main.graph, "\Smith chart Plotter"
    print#main.graph, "\";date$()
    print#main.graph, "flush"
end sub
'------------------------------------------------------------------------------------------------------------------------------------
'                               end os sub
'-------------------------------------------------------------------------------------------------------------------------------------

'----------------------------------------------------------------------
'                    snapshot section
'----------------------------------------------------------------------

[snap]
    if flag = 0 then notice "Graphics field is empty! no picture to snap for": wait
    filedialog "Save As...", "*.bmp", fileName$
    timer 800, [down]
    wait
    [down]
    print#main.graph, "getbmp snap 6, 0, 694, 638"
    bmpsave "snap", fileName$; ".bmp"
    timer 0
    run "C:\WINDOWS\system32\mspaint.exe ";fileName$;".bmp"
    unloadbmp "snap"
    wait
'___________________________________________________________________________'
'         "Quit the App " section
'
'___________________________________________________________________________'

[Quit]
    confirm "Are you sure you want to quit ?"; ch$
    if instr("NOno", ch$) > 0 then goto [main.inputLoop]

[quitFinal]
    close #main
    end
'____________________________________________________________________________'
'             END OF THE PROGRAM                                                                                                                       '
'____________________________________________________________________________














