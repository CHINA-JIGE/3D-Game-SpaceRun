Attribute VB_Name = "移动系统"

Sub f移动系统()
10000        If TV.TimeElapsed > 50 Then GoTo ex: '突然一大卡就离开


10015        If Vcam < 25 Then Vcam = Vcam + 0.0001 * TV.TimeElapsed


10030        Scam = camX



10050        inputE.GetMouseState tmpmouseX, tmpmouseY


10065        RotateX = RotateX + tmpmouseX * TV.TimeElapsed / 10000 * GameSettings.MouseSensitivity * TimePassSpeed '鼠标移动 镜头旋转
             '灵敏度默认值为 1
10075        RotateY = RotateY - tmpmouseY * TV.TimeElapsed / 10000 * GameSettings.MouseSensitivity * TimePassSpeed

10085        Select Case RotateY

                    Case Is > 1

10105                      RotateY = 1

10115               Case Is < -1

10125                      RotateY = -1

10135        End Select


10150        Select Case RotateX

                    Case Is > 2.3

10170                      RotateX = 2.3

10180               Case Is < 0.8

10190                      RotateX = 0.8

10200        End Select


10215        DeltaVConst = (TV.TimeElapsed ^ 2) * 0.02 * F阻力

 
10230        If inputE.IsKeyPressed(TV_KEY_W) = True And HasJumped < 2 And CanPressW = True Then '跳起
10235               HasJumped = HasJumped + 1 '已跳次数
10240               CanPressW = False
10245               If NoGravityActivated = False Then
10250                      VY = 0.4 * TV.TimeElapsed  '不是无重力就跳
10255               Else
10260                      VY = (VY + TV.TimeElapsed ^ 2 / 1200)
10265               End If
10270        End If
 
 
 
10290        If inputE.IsKeyPressed(TV_KEY_A) = True Then ' And MoveToLaneActivated = False And HasPressA = False Then
10295               VR = VR - DeltaVConst / 2 ' DeltaVConst / 5必须比下面减速度要大
10300        End If


10315        If inputE.IsKeyPressed(TV_KEY_D) = True Then  ' And MoveToLaneActivated = False And HasPressD = False Then
10320               VR = VR + DeltaVConst / 2
10325        End If



10345        If CanPressW = False Then

10355               HasPressWTime = HasPressWTime + TV.TimeElapsed

10365               If HasPressWTime > 500 Then
    
10375                      CanPressW = True
    
10385                      HasPressWTime = 0
    
10395               End If
    
10405        End If

             'End If
             '///////////////////////////////////////////////////////



             ' If VR = 0 Then
             ' MoveToLaneActivated = False
             '     If inputE.IsKeyPressed(TV_KEY_A) = False Then HasPressA = False
             '     If inputE.IsKeyPressed(TV_KEY_D) = False Then HasPressD = False
             '  End If

             'Select Case VUp '限制速度 &减速度
             'Case Is > TV.TimeElapsed * 1.5
             'VUp = TV.TimeElapsed * 1.5
             'Case Is < -TV.TimeElapsed * 1.5
             'VUp = -TV.TimeElapsed * 1.5
             'Case Is > 0
             'VUp = VUp - DeltaVConst
             '    If IsJumping = True Then VUp = VUp + DeltaVConst  '空中加的不要那么多啊
             'If VUp < 0 Then VUp = 0
             'Case Is < 0
             'VUp = VUp + DeltaVConst
             '    If IsJumping = True Then VUp = VUp - DeltaVConst
             'If VUp > 0 Then VUp = 0
             'End Select




10560        Select Case VR '限制速度 &减速度

                    Case Is > TV.TimeElapsed * 3
10575                      VR = TV.TimeElapsed * 3


10590               Case Is < -TV.TimeElapsed * 3
10595                      VR = -TV.TimeElapsed * 3


10610               Case Is > 0 '左右的速度
10615                      VR = VR - DeltaVConst / 5
10620                      If VR < 0 Then VR = 0


10635               Case Is < 0
10640                      VR = VR + DeltaVConst / 5
                           ' If scene.Collision(Vector(camX, camY, camZ), Vector(camX, camY - 30, camZ)) = False Then VR = VR - DeltaVConst / 6
10650                      If VR > 0 Then VR = 0

10660        End Select


10675        If VY < -3 * TV.TimeElapsed Then VY = -3 * TV.TimeElapsed

10685        If VY > 3 * TV.TimeElapsed Then VY = 3 * TV.TimeElapsed
  
  
  
  
  
  
  
  
  
  
  
  
  
  

             '////////////////////////////////////////////////////////
  
             '//////////////////////处理坐标/////////////////////////

             '。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。

             '。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。
10800        For i = 1 To 30

                    '撞到小陨石就扣血
10815               If GetDistance(camX, camY, camZ, Asteroid(i).GetPosition.X, Asteroid(i).GetPosition.Y, Asteroid(i).GetPosition.Z) _
                       < 200 * Asteroid(i).GetScale.X Then

                           ' 与体积平方成正比
10835                      HP = HP - Asteroid(i).GetScale.X ^ 2 * 100

10845                      GE.Flash 1, 0, 0, 300

10855                      SetTextAlpha text_撞到陨石, 1
                           '放远点
10865                      Asteroid(i).MoveRelative 6000, 0, 0

10875               End If

10885        Next i









             '所有可碰撞的东西的测试
10940        With scene

                    '纵向速度 正常情况
10955               If NoGravityActivated = False Then VY = VY - TV.TimeElapsed ^ 2 * TimePassSpeed / 1800 '可为负数
                    '加速度要等于timeElapsed的平方
                    '用加速度 速度 和数列去推导
                    '1s所走的Y坐标路程要为定值 不能关于TimeElapsed
 
10980               If .Collision(Vector(camX, camY, camZ), Vector(camX, camY - 50, camZ)) = True Then '着陆了

                           '////////////落地速度 扣血
10995                      If VY < -TV.TimeElapsed Then
        
11005                             SetTextAlpha text_摔着了, 1
        
11015                             GE.Flash 1, 0, 0, 300
        
11025                             HP = HP + VY '太高摔下要减血
        
11035                      End If
        
                           '//////////////着陆行走的时候小小跳一下 更容易起跳 因为设定了离地50单位是着陆
                           '激光只横向碰撞
11055                      NGALaser.SetCollisionEnable False

11065                      VY = 0.05 * TV.TimeElapsed



11085                      If HasJumped > 0 And inputE.IsKeyPressed(TV_KEY_W) = False Then '已跳 着陆
    
11095                             VY = 0.5 / (TimePassSpeed ^ 2)
    
11105                             HasJumped = 0
    
11115                      End If
    
    
                           '/////////还没跳够两次
11135                      If HasJumped < 2 And inputE.IsKeyPressed(TV_KEY_W) = True And CanPressW = True Then
    
11145                             VY = 0.4 * TV.TimeElapsed / (TimePassSpeed ^ 2)
    
11155                      End If
    
    
    

11180                      If .Collision(Vector(camX, camY, camZ), Vector(camX, camY - 59.5, camZ)) = True Then
    
11190                             camY = camY + 3
        
11200                             VY = VY + 0.5
    
11210                      End If
    
    
11225               Else '没着陆
                           'Key_S 坠落
11235                      If inputE.IsKeyPressed(TV_KEY_S) = True Then

11245                             If NoGravityActivated = False Then
    
11255                                    VY = VY - TV.TimeElapsed ^ 2 * (TimePassSpeed) / 1800
    
11265                             Else
    
11275                                    VY = VY - TV.TimeElapsed ^ 2 * (TimePassSpeed) / 1200
        
11285                             End If
    
11295                      End If

11305               End If

 
 
11325               DeltaCamZ = VR / 5 '(Vcam * Cos(RotateX) - VR * Sin(RotateX)) / 5


11340               NGALaser.SetCollisionEnable True
                    '速度处理
11350               If .Collision(Vector(camX, camY, camZ), Vector(camX + Vcam * TV.TimeElapsed / 10, camY, camZ)) = False Then

11360                      camX = camX + Vcam * TimePassSpeed * TV.TimeElapsed / 20                       'sgn()正数是1，负数0  vcam * TV.TimeElapsed / 10大致说的是X坐标的增加，判断下一步的合理性

11370               Else

11380                      IsDead = True

11390               End If



                    'Z向检测
11415               If .Collision(Vector(camX, camY, camZ), Vector(camX, camY, camZ + (Sgn(DeltaCamZ) - 0.5) * 5)) = False Then
                           '下一帧Z
11425                      camZ = camZ - DeltaCamZ * TimePassSpeed

11435                      Else: IsDead = True

11445               End If



                    '头顶撞了
11470               If .Collision(Vector(camX, camY, camZ), Vector(camX, camY + 2, camZ)) = True Then

11480                      IsDead = True

11490               End If


11505        End With



             '在一开始的隧道那
11530        If StartTunnel.Collision(Vector(camX, camY, camZ), Vector(camX, camY - 80, camZ), TV_TESTTYPE_DEFAULT) = True Then

11540               VY = 0

11550               HasJumped = 0

11560               CanPressW = True

11570        End If

             '。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。

             '。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。

             '。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。

             '。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。








11655        CamLX = camX + 200 * Sin(RotateX)
11660        CamLY = camY + 200 * Sin(RotateY)
11665        CamLZ = camZ + 200 * Cos(RotateX)

11675        camY = camY + VY * TimePassSpeed
11680        CamLY = CamLY + VY * TimePassSpeed

             'WriteLog ("vy:" & CStr(VY) & "   " & CStr(TV.GetFPS))



             '///////////////放慢镜的临界高度
11715        If camY < -1000 Then
11720               TimePassSpeed = 0.1
11725               If GameSettings.IsSoundOn = True Then
11730                      GameSound.HeartBeat.Play
11735               End If
11740               GameBGM(GameBGMID).Volume = -2000
11745        End If


             '/////////////死亡临界高度
11765        If camY < -2000 Or HP <= 0 Then
11770               IsDead = True
11775        End If



11795        If IsDead = True Then
11800               cam.SetViewFrustum 60, 7000
11805               HP = 0
11810               GameBGM(GameBGMID).Volume = -800
11815               If GameSettings.IsSoundOn = True Then GameSound.Scream.Play
11820               If GameSettings.IsSoundOn = True Then GameSound.HeartBeat.Stop_
                    '为了之后的按钮能准确判断
11830               TV.ResizeDevice

11840        End If



11860        cam.SetCamera camX, camY, camZ, CamLX, CamLY, CamLZ

11870 ex:
End Sub















