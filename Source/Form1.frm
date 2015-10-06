VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "SpaceRun"
   ClientHeight    =   10785
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   19545
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   10785
   ScaleWidth      =   19545
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  '所有者中心
   WindowState     =   2  'Maximized
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000001&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   9735
      Left            =   0
      ScaleHeight     =   10000
      ScaleMode       =   0  'User
      ScaleWidth      =   16995
      TabIndex        =   0
      Top             =   0
      Width           =   17000
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'开头动画
Private StartAnimCountTime As Single
Private Sub Form_Load()
If InStr(1, CStr(App.Path), " ") > 0 Then
MsgBox "错误！绝对路径不能包含空格(整个路径)，不然注册会失败，程序即将退出！", vbCritical, "路径错误"
End
End If

  '没注册过 拿个文件做标记
If Dir(App.Path & "\RegMarkE.m") = "" Then
    Shell "regsvr32.EXE " & App.Path & "\TV3D65.DLL", vbNormalFocus
    Open App.Path & "\RegMarkE.m" For Append As #1
    Print #1, "Marked"
    Close #1
End If

If Dir(App.Path & "\RegMarkM.m") = "" Then
    Shell "regsvr32.EXE " & App.Path & "\tvmedia.DLL", vbNormalFocus
    Open App.Path & "\RegMarkM.m" For Append As #1
    Print #1, "Marked"
    Close #1
End If

'如果是第一次运行游戏那就安装字体
Dim AddFontResult As Long
If Dir(App.Path & "\FontMark.m") = "" Then
AddFontResult = AddFontResource(App.Path & "\华康娃娃体.TTF")
    If AddFontResult <> 0 Then
    MsgBox "字体应该安装成功！"
    Open App.Path & "\FontMark.m" For Append As #2
    Print #2, "Marked"
    Close #2
    End If
End If






Form1.Show

'容器设为全屏那么大 TV加载的窗口大小是全屏
Picture1.Width = Form1.Width

Picture1.Height = Form1.Height

f初始化





'///////////////////////////////////////////////////////////
'/////////////////////////////主循环///////////////////////
'////////////////////////////////////////////////////////

Do


DoEvents


Form1.Caption = GameTools.Magnet_Mesh.GetPosition.X & "camx :" & camX  'camX & "   y:" & camY & "   z:" & camZ


Select Case Playlevel


Case 1


TV.Clear

For a = 1 To 4

If Button(a).MouseHasClick = True Then

    Select Case a
    
    Case 1 '开始游戏
    NextPlayLevelToGo = 2
    
    
    Case 2 '商店
    NextPlayLevelToGo = 4
    
    Case 3 '设置
    NextPlayLevelToGo = 5
    PlayLevelBeforeSetting = 1 '说明是从<设置>进入的 '可能是从游戏中进入设置 也可能是主菜单进入
    
    Case 4 '退出
    NextPlayLevelToGo = 100
    
    
    
    End Select
    
    If NextPlayLevelToGo <> 0 Then GE.FadeOut 500

End If

Next a




    '先淡出吧有feel点
If NextPlayLevelToGo <> 0 And GE.IsFadeFinished = True Then

Playlevel = NextPlayLevelToGo 'NextPlayLevelToGo在之前用设置好噢

    If NextPlayLevelToGo = 2 Then '游戏主体
         Picture1.Width = 17000
         Picture1.Height = 10000
         'TV要更新设备大小 不然2D绘图的坐标什么的会有偏差
          TV.ResizeDevice
         'TV加载好了再把容器调为全屏
         Picture1.Width = Form1.Width
         Picture1.Height = Form1.Height
         
       MenuBGM.Stop_
    If GameSettings.IsSoundOn = True Then GameBGM(GameBGMID).Play
    End If
         
NextPlayLevelToGo = 0


GE.FadeIn 1000
    
End If


















Case 2 '游戏中
'/////////歌曲切换与循环
KeyCountTime.Key_PageDown = KeyCountTime.Key_PageDown + TV.TimeElapsed

'播完一首 或  按了PAGEUP换歌
If (GameBGM(GameBGMID).PlayState = TV_PLAYSTATE_ENDED) Or _
(KeyCountTime.Key_PageDown > 2000 And inputE.IsKeyPressed(TV_KEY_PAGEDOWN) = True) Then
GameBGM(GameBGMID).Stop_
KeyCountTime.Key_PageDown = 0
GameBGMID = GameBGMID + 1
       If GameBGMID > 3 Then
       GameBGMID = 1
       End If
'音效开了就播放下一首
If GameSettings.IsSoundOn = True Then GameBGM(GameBGMID).Play
End If



TV.Clear

If inputE.IsKeyPressed(TV_KEY_ESCAPE) = True Then
Playlevel = 6 '暂停
TV.ResizeDevice
End If


If SE.EventActivated = False Then '为了配合特殊事件3 camera的degree


    If CamViewDegree > 60 Then CamViewDegree = CamViewDegree - 0.02 * TV.TimeElapsed


    If CamViewDegree < 63 Then CamViewDegree = 60 '在特殊事件3 加速隧道 会有这个镜头变换


    cam.SetViewFrustum CamViewDegree, 7000
    
    
End If



f移动系统


特殊事件


道具与存钱跑道


f重置跑道 '顺便重置RMB


'星球放大
SunSize = SunSize + 0.0001 * TV.TimeElapsed


If SunSize < 6 Then atmo.Sun_SetBillboardSize SunSize

'RMB旋转
RMBRotateY = RMBRotateY + 1 * TimePassSpeed


    For r = 1 To 70
    
    
    RMB(r).SetRotation 0, RMBRotateY, 0
    
    
    PickMoneyParticle(r).Update
    
    
    Next r

'陨石碎
LittleRock.SetGlobalPosition camX + 2000, 150, 500  '粒子系统的更新


LittleRock.Update


If TotalCash > 10000000 Then TotalCash = 9999999


f渲染 (1)



If IsDead = True Then '////////////死掉

If GameScore > HighScore Then HighScore = Int(GameScore)


ShockActivated = True


ShockTimeOnce = 0


ShockTime = 0


cam.SetViewFrustum 60, 7000


GE.Flash 1, 0, 0, 1500


Playlevel = 3


数据存档 (DataType_PlayerData)


End If


















Case 3 '死掉


TV.Clear


f震屏 1000, 10

'////////////////////camera开始挪
camX = camX + 0.5 * TV.TimeElapsed

CamLX = camX + 1000

CamLY = camY - 500

CamLZ = camZ + 1500

If camY < 500 Then camY = camY + 5 * TV.TimeElapsed

If camZ > -1000 Then camZ = camZ - 5 * TV.TimeElapsed

cam.SetCamera camX, camY, camZ, CamLX, CamLY, CamLZ



RMBRotateY = RMBRotateY + 1


    For r = 1 To 70
    
    
    RMB(r).SetRotation 0, RMBRotateY, 0
    
    
    Next r
    
    
f重置跑道


LittleRock.SetGlobalPosition camX + 2000, 150, 1000


LittleRock.Update


For i = 1 To 30
ExplosionParticle(i).ResetAll
Next i

f渲染 (1)


'///////////////////////死后菜单的3个按钮
For a = 5 To 7

If Button(a).MouseHasClick = True Then

    Select Case a
    
    Case 5 '重新开始
    NextPlayLevelToGo = 2
    
    
    Case 6 '商店
    NextPlayLevelToGo = 4
    
    
    Case 7 '返回主菜单
       NextPlayLevelToGo = 1
    
    End Select
    
    '每次设置完目标level之后要清空
    
    If NextPlayLevelToGo <> 0 Then GE.FadeOut 500


End If

Next a


If NextPlayLevelToGo <> 0 And GE.IsFadeFinished = True Then
Playlevel = NextPlayLevelToGo 'NextPlayLevelToGo在之前用设置好噢
       If NextPlayLevelToGo = 2 Then '游戏主体
              Picture1.Width = 17000
              Picture1.Height = 10000
              'TV要更新设备大小 不然2D绘图的坐标什么的会有偏差
              TV.ResizeDevice
              'TV加载好了再把容器调为全屏
              Picture1.Width = Form1.Width
              Picture1.Height = Form1.Height
       End If
    
       If NextPlayLevelToGo = 1 Then '从游戏返回主菜单换歌
              For i = 1 To 3
              GameBGM(i).Stop_
              Next i
              MenuBGM.Play
       End If
         
NextPlayLevelToGo = 0

重新初始化参数

GE.FadeIn 500

End If













'//////////////////////////商店
'//////////////////////////商店
'//////////////////////////商店
Case 4

TV.Clear

'背景
SCR.Draw_Texture GetTex("ShopBG"), 0, 0, FormWidth, FormHeight

'防止一下子买了一堆
AfterPressCountTime = AfterPressCountTime + TV.TimeElapsed

For a = 8 To 11

'按完0.5秒才能买下一次
If Button(a).MouseStatus = Button_IsClicked Then


    Select Case a
    Case 8 '回血
        If TotalCash > 3000 And GameTools.HP_InShop_TheNumberPlayerHas <= 9 And _
        AfterPressCountTime > 300 Then
              TotalCash = TotalCash - 3000
              AfterPressCountTime = 0
              GameTools.HP_InShop_TheNumberPlayerHas = GameTools.HP_InShop_TheNumberPlayerHas + 1
              Else
              SCRText.NormalFont_DrawText "余额不足或道具够多了。", 500, 560, RGBA(1, 0.6, 0.6, 1), 3
        End If
    
    Case 9 '弹射系统
        If TotalCash > 6000 And GameTools.Eject_TheNumberPlayerHas <= 4 And _
        AfterPressCountTime > 300 Then
              TotalCash = TotalCash - 6000
              AfterPressCountTime = 0
              GameTools.Eject_TheNumberPlayerHas = GameTools.Eject_TheNumberPlayerHas + 1
              Else
              SCRText.NormalFont_DrawText "余额不足或道具够多了。", 500, 560, RGBA(1, 0.6, 0.6, 1), 3
        End If
        
    Case 10 '远程存money
        If TotalCash > 4000 And GameTools.MoneySave_TheNumberPlayerHas <= 4 And _
        AfterPressCountTime > 300 Then
              TotalCash = TotalCash - 4000
              AfterPressCountTime = 0
              GameTools.MoneySave_TheNumberPlayerHas = GameTools.MoneySave_TheNumberPlayerHas + 1
              Else
              SCRText.NormalFont_DrawText "余额不足或道具够多了。", 500, 560, RGBA(1, 0.6, 0.6, 1), 3
        End If
        
    
    Case 11 '返回主菜单
    '//////BGM切换
    If GameSettings.IsSoundOn = True Then
    MenuBGM.Play
    GameBGM(GameBGMID).Stop_
    End If
    
    NextPlayLevelToGo = 1
    数据存档 (DataType_PlayerData)
    GE.FadeOut 500
       
    
    End Select

End If

Next a



If NextPlayLevelToGo <> 0 And GE.IsFadeFinished = True Then

Playlevel = NextPlayLevelToGo 'NextPlayLevelToGo在之前用设置好噢
         
NextPlayLevelToGo = 0

重新初始化参数

GE.FadeIn 500

       If NextPlayLevelToGo = 1 Then '从游戏返回主菜单换歌
              For i = 1 To 5
              GameBGM(i).Stop_
              Next i
              MenuBGM.Play
       End If

End If












'////////////////////////////////////设置////////////////////////////////////
'////////////////////////////////////设置////////////////////////////////////
'////////////////////////////////////设置////////////////////////////////////
Case 5

TV.Clear

SCR.Draw_Texture GetTex("ShopBG"), 0, 0, FormWidth, FormHeight

SCRText.NormalFont_DrawText "设   置", 500, 50, RGBA(1, 0.8, 0.4, 1), 2

If Button(12).MouseHasClick = True And GameSettings.MouseSensitivity > 0.5 Then '减灵敏度
GameSettings.MouseSensitivity = GameSettings.MouseSensitivity - 0.0005 * TV.TimeElapsed
End If
If Button(13).MouseHasClick = True And GameSettings.MouseSensitivity < 2 Then '加灵敏度
GameSettings.MouseSensitivity = GameSettings.MouseSensitivity + 0.0005 * TV.TimeElapsed
End If

'音效开关
If Button(14).MouseHasClick = True Then
GameSettings.IsSoundOn = True '音效开
              If PlayLevelBeforeSetting = 6 Then GameBGM(GameBGMID).Play '从游戏的设置进入的就播放
End If

If Button(15).MouseHasClick = True Then
GameSettings.IsSoundOn = False '音效关
              If PlayLevelBeforeSetting = 6 Then GameBGM(GameBGMID).Pause
End If
       
If Button(16).MouseHasClick = True Then
       NextPlayLevelToGo = 1 '先让他不为零 然后就满足了滚走的条件
    数据存档 (DataType_SettingData)
    GE.FadeOut 500
End If


If NextPlayLevelToGo <> 0 And GE.IsFadeFinished = True Then
Playlevel = PlayLevelBeforeSetting '返回进入的地方 主游戏或主菜单
NextPlayLevelToGo = 0
GE.FadeIn 500
End If

       








'///////////////////////////////////////////暂停菜单
Case 6
TV.Clear

'暂停菜单
For i = 17 To 19
UI按钮判断 (i)
Next i

If Button(17).MouseStatus = Button_IsClicked Then
Playlevel = 2
Picture1.Width = 17000
Picture1.Height = 10000
TV.ResizeDevice
Picture1.Width = Form1.Width
Picture1.Height = Form1.Height

End If


If Button(18).MouseStatus = Button_IsClicked Then
NextPlayLevelToGo = 5
PlayLevelBeforeSetting = 6 '说明是从<设置>进入的 '可能是从游戏中进入设置 也可能是主菜单进入
GE.FadeOut 500
End If

If Button(19).MouseStatus = Button_IsClicked Then
NextPlayLevelToGo = 1 '返回主菜单
GE.FadeOut 500
End If


If NextPlayLevelToGo <> 0 And GE.IsFadeFinished = True Then
              If NextPlayLevelToGo = 1 Then '换歌
                     For i = 1 To 3
                     GameBGM(i).Stop_
                     Next i
              MenuBGM.Play
              End If
Playlevel = NextPlayLevelToGo 'NextPlayLevelToGo在之前用设置好噢
NextPlayLevelToGo = 0
GE.FadeIn 500
End If







'退出
Case 100
Form_Unload (0)




'程序开头“动画”
Case 101
TV.Clear
StartAnimCountTime = StartAnimCountTime + TV.TimeElapsed
       Select Case StartAnimCountTime
       Case 0 To 7900
       SCR.Draw_Texture GetTex("MenuTitle"), FormCenterX - 200, FormCenterY - 300, FormCenterX + 200, FormCenterY - 50
       SCRText.NormalFont_DrawText "Made By X.X.O.X.X", FormCenterX - 150, FormCenterY, RGBA(1, 1, 1, 1), 1
       SCRText.NormalFont_DrawText "新浪微博:@鸡哥表示名字不会再爆格-爱执信", FormCenterX - 350, FormCenterY + 100, RGBA(1, 1, 1, 1), 1
       
       Case 7990 To 8000
       GE.FadeIn 1000
       Case 8000 To 10000
       SCRText.NormalFont_DrawText "开发环境:Visual Basic", FormCenterX - 150, FormCenterY - 50, RGBA(1, 1, 1, 1), 1
       SCRText.NormalFont_DrawText "3D引擎:TrueVision3D SDK 6.5", FormCenterX - 250, FormCenterY + 50, RGBA(1, 1, 1, 1), 1
       
       Case Is > 10000
       GE.FadeIn 1000
       Playlevel = 1
       End Select
       
End Select

















'处理完移动系统再确定屏幕中心 camlx要改变 【其实顺序应该无所谓的= =
SCR.Math_3DPointTo2D Vector(CamLX, CamLY, CamLZ), FormCenterX, FormCenterY, True

FormWidth = Int(FormCenterX) * 2

FormHeight = Int(FormCenterY) * 2


绘图2D


TV.RenderToScreen


'//////////////截屏
KeyCountTime.Key_PrintScreen = KeyCountTime.Key_PrintScreen + TV.TimeElapsed
If inputE.IsKeyPressed(TV_KEY_SYSRQ) = True And KeyCountTime.Key_PrintScreen > 1000 Then
GE.Flash 1, 1, 1, 500
KeyCountTime.Key_PrintScreen = 0
SetTextAlpha text_已截屏, 1

TV.Screenshot App.Path & "\游戏截图\SpaceRun" & Replace(CStr(Date), "/", " ") _
& Replace(CStr(Time), ":", " ") & ".jpg", TV_IMAGE_JPG
End If




Loop



TV.ReleaseAll


End


End Sub




































Sub f渲染(PL As Integer)
Select Case PL

Case 1

TV.Clear


'DeepRS.StartRender
atmo.Atmosphere_Render

StartTunnel.Render


For r = 1 To 15
NRWay(r).Render
Next


For a = 1 To 30
Asteroid(a).Render
Next


'存钱跑道
MoneySaveRunway.Render

'道具 磁铁
GameTools.Magnet_Mesh.Render
GameTools.TimeSlow_Mesh.Render
GameTools.RecoverHP_Mesh.Render


If SE.EventActivated = True Then


    For a = 1 To 30
    
    
    FR(a).Mesh.Render
    
    
    ExplosionParticle(a).Render '落石
    
    
    Next
    
    
SpeedUpTunnel.Render '加减速


SpeedDownTunnel.Render


SpeedTunnelRing(1).Render


SpeedTunnelRing(2).Render


SpeedUpParticle.Update
SpeedUpParticle.Render


SpeedDownParticle.Update
SpeedDownParticle.Render


NoGravityArea.Update '无重力区
NoGravityArea.Render



NGALaser.Render


End If

LittleRock.Render

For r = 1 To 70 '爆出的money粒子系统  摆在最上层渲染
RMB(r).Render
PickMoneyParticle(r).Update
PickMoneyParticle(r).Render
Next



   



End Select

End Sub



Private Sub Form_Unload(Cancel As Integer)
TV.ReleaseAll
End
End Sub


