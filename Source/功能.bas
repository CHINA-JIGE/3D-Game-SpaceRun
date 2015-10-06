Attribute VB_Name = "功能"

Public Function GetDistance(x1 As Single, y1 As Single, z1 As Single, x2 As Single, y2 As Single, z2 As Single)


XD = Abs(x1 - x2)


YD = Abs(y1 - y2)


ZD = Abs(z1 - z2)


D水平面 = Sqr(XD ^ 2 + ZD ^ 2)


GetDistance = Sqr(D水平面 ^ 2 + YD ^ 2)


End Function













'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////

Public Function MoveCam(DirectionX As Single, DirectionY As Single, DirectionZ As Single, DLookatX As Single, DlookatY As Single, DlookatZ As Single, CamTime As Single, LookatTime As Single)


If TV.TimeElapsed / CamTime > 1 Then cantime = 1


 CamMoveX = (DirectionX - camX) * TV.TimeElapsed / CamTime
 
 
 CamMoveY = (DirectionY - camY) * TV.TimeElapsed / CamTime
 
 
 CamMoveZ = (DirectionZ - camZ) * TV.TimeElapsed / CamTime


 CamMoveLX = (DLookatX - CamLX) * TV.TimeElapsed / LookatTime
 
 
 CamMoveLY = (DlookatY - CamLY) * TV.TimeElapsed / LookatTime
 
 
 CamMoveLZ = (DlookatZ - CamLZ) * TV.TimeElapsed / LookatTime


If GetDistance(camX, camY, camZ, DirectionX, DirectionY, DirectionZ) < 5 Then


CamPosMoveFinish = True


camX = DirectionX


camY = DirectionY


camZ = DirectionZ


Else: CamPosMoveFinish = False


End If

If GetDistance(CamLX, CamLY, CamLZ, DLookatX, DlookatY, DlookatZ) < 5 Then


CamLookatMoveFinish = True


Else: CamLookatMoveFinish = False


End If


If CamPosMoveFinish = False Then


camX = camX + CamMoveX


camY = camY + CamMoveY


camZ = camZ + CamMoveZ


End If


If CamLookatMoveFinish = False Then


CamLX = CamLX + CamMoveLX


CamLY = CamLY + CamMoveLY


CamLZ = CamLZ + CamMoveLZ


End If


cam.SetCamera camX, camY, camZ, CamLX, CamLY, CamLZ


End Function






























'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
Public Function WriteLog(WString As String)


Open "d:\TV3D.log" For Append As #1


Print #1, CStr(WString)


Close #1


End Function










'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'Public Sub RefreshExplosionAnim(RefreshTime As Single, ExpAnimId As Variant) '废掉的


'ExpAnimId = Val(ExpAnimId)


'If ExpAnim(ExpAnimId).Activated = True Then
'被激活的话呢

'        ExpAnim(ExpAnimId).CountTime = ExpAnim(ExpAnimId).CountTime + TV.TimeElapsed
        '计时器计时吧！

'        If ExpAnim(ExpAnimId).CountTime > RefreshTime Then  '计时器到点 还要动画是被激活的


'        ExpAnim(ExpAnimId).TextureID = ExpAnim(ExpAnimId).TextureID + 1


'        ExpAnim(ExpAnimId).CountTime = 0 '单独的计时器 归零


'                If ExpAnim(ExpAnimId).TextureID = 8 Then '第8张图 /播完了
    
    
'                ExpAnim(ExpAnimId).TextureID = 0
    
    
'                ExpAnim(ExpAnimId).Activated = False
    
                
'                ExplosionAnimation(ExpAnimId).SetPosition 0, 0, 0 '播完就扔走吧
    
'                End If
   '因为EXPL1 -8是顺着加载的所以ID也是连起的

'        End If
        
'End If
'ExplosionAnimation(ExpAnimId).SetTexture GetTex("expl1") + ExpAnim(ExpAnimId).TextureID
'End Sub












'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
Public Function GetTextAlpha(ScrTextID As ScrTextName) As Single
GetTextAlpha = ScrTextAlpha(ScrTextID).Alpha
End Function

Public Sub SetTextAlpha(ScrTextID As ScrTextName, AlphaToSet As Single)
ScrTextAlpha(ScrTextID).Alpha = AlphaToSet
End Sub

Public Function GetTextSpeed(nScrTextID As ScrTextName) As Single
GetTextSpeed = ScrTextAlpha(nScrTextID).FadeOutSpeed
End Function
'If inputE.IsKeyPressed(TV_KEY_LEFTCONTROL) = True Then



















'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////

Public Sub f震屏(ShockUntil As Single, ShockRange As Single) ' ShockFrequency As Single) '输入要震的时间
'用的时候记得激活 两个计时参数归零

Randomize


ShockTimeOnce = ShockTimeOnce + TV.TimeElapsed '计时 单次的 够钟就归零


ShockTime = ShockTime + TV.TimeElapsed '总时间几时 够钟就走人


If ShockActivated = True Then



        If ShockTimeOnce > 30 / TimePassSpeed Then
        

Form1.Picture1.Left = (-2 * ShockRange * Rnd(1) + ShockRange) * 15 '像素和VB坐标的转换


Form1.Picture1.Top = (-2 * ShockRange * Rnd(1) + ShockRange) * 15

        

        ShockTimeOnce = 0


        End If
        
        
End If




If ShockTime > ShockUntil Then


ShockActivated = False


ShockTime = 0


ShockTimeOnce = 0


Form1.Picture1.Left = 0


Form1.Picture1.Top = 0


End If


End Sub












'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////

Public Sub 重置金币(ResetMoneyMode As RMMode)

'money过一秒不拿就播最低音
'CoinSoundCountTime = CoinSoundCountTime + TV.TimeElapsed


If ResetMoneyMode = 0 Then '按规律地重置


    For r = 1 To 70
    
    
        If GetDistance(camX, camY, camZ, RmbPosX(r), RMB(r).GetPosition.Y, RmbPosZ(r)) < 30 Then '拿到金币
        '////////音效

       If GameSettings.IsSoundOn = True Then GameSound.Coin(r).Play
              
        
        '///////失去磁性
            RmbIsMagnetic(r) = False
        
        '、、、、、加钱
            UnSavedCashInGame = UnSavedCashInGame + 50 * MoneyMultiplier
            
        '/////////////加分
            GameScore = GameScore + 5 * TV.TimeElapsed * MoneyMultiplier
        
        
        '、、、、、、、、、、可以写字了
            SetTextAlpha Text_人民币50, 1
        
        
            PickMoneyParticle(r).ResetAll '、、、、、、、、、捡钱爆出点东西
            
            '在渲染那里update 爆钱的地方大约在camera正前方
            PickMoneyParticle(r).SetGlobalPosition RmbPosX(r) + Vcam * 15, RMB(r).GetPosition.Y + 20, RmbPosZ(r) 'RmbPosX(r), RMB(r).GetPosition.Y, RmbPosZ(r)
        
        
       ' GE.Flash 0.6, 1, 0.6, 300
        
        
        
            RmbPosZ(r) = NRWay(r Mod 14 + 1).GetPosition.Z - 50 + 100 * Rnd(1) '
        
        
            RmbPosX(r) = NRWay(r Mod 14 + 1).GetPosition.X + (70 * (r Mod 5))
        
        
            RMB(r).SetPosition RmbPosX(r), NRWay((r Mod 14) + 1).GetPosition.Y + 20, RmbPosZ(r)
        
        
        End If


        If camX > RmbPosX(r) Then  '越过money了
        
        
            RmbIsMagnetic(r) = False
        
        
            RmbPosZ(r) = NRWay(r Mod 14 + 1).GetPosition.Z - 50 + 100 * Rnd(1) '
        
        
            RmbPosX(r) = NRWay(r Mod 14 + 1).GetPosition.X + (70 * (r Mod 5))
        
        
            RMB(r).SetPosition RmbPosX(r), NRWay((r Mod 14) + 1).GetPosition.Y + 70, RmbPosZ(r) '重置money位置 在每个岩石跑道上 随机抽取Z 离地30
        
        
        End If
    
    
    Next r
    
    
End If









If ResetMoneyMode = 1 Then


    For r = 1 To 70
        
        
            If GetDistance(camX, camY, camZ, RmbPosX(r), RMB(r).GetPosition.Y, RmbPosZ(r)) < 50 Then '拿到金币
            

               If GameSettings.IsSoundOn = True Then GameSound.Coin(r).Play
            
            '不受磁铁控制
                RmbIsMagnetic(r) = False
            
            
                PickMoneyParticle(r).ResetAll '捡钱爆出点东西
            
            '在渲染那里update
                PickMoneyParticle(r).SetGlobalPosition camX + Vcam * 15, RMB(r).GetPosition.Y + 20, RmbPosZ(r)
            
                '、、、、、、、、、、、、加钱
                UnSavedCashInGame = UnSavedCashInGame + 50 * MoneyMultiplier '加钱
            
                '//////////加分
                GameScore = GameScore + 5 * TV.TimeElapsed * MoneyMultiplier
            
            
                RmbPosZ(r) = 1000 + 50 * Rnd(1) '在一定空间范围内
            
            
                RmbPosX(r) = camX + 4000 + 2000 * Rnd(1)
            
            
                RMB(r).SetPosition RmbPosX(r), 300 + 50 * Rnd(1), RmbPosZ(r)
            
            
            End If
            
            
            If camX > RmbPosX(r) Then
            
                '不受磁铁控制
                RmbIsMagnetic(r) = False
            
            
                RmbPosZ(r) = 1000 + 50 * Rnd(1) '在一定空间范围内
            
            
                RmbPosX(r) = camX + 4000 + 2000 * Rnd(1)
            
            
                RMB(r).SetPosition RmbPosX(r), 300 + 50 * Rnd(1), RmbPosZ(r)
            
            
            End If
            
            
    Next r
        
        
End If




'拿够一千块
If UnSavedCashInGame Mod 1000 < 30 And UnSavedCashInGame <> 0 Then
    
    UnSavedCashInGame = UnSavedCashInGame + 50
    
    '闪下绿
    GE.Flash 0.6, 1, 0.6, 200
    
    SetTextAlpha Text_人民币1000, 1
    
End If


End Sub









'////////////////////////////////////////////////////////////
'////////////////////////////////////////////////////////////
'////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
'////////////////////////////////////////////////////////////

Public Sub MoveRMB(RmbID As Long, DestinationX As Single, DestinationY As Single, DestinationZ As Single)


DeltaRmbMoveX = (DestinationX - RmbPosX(RmbID)) * TV.TimeElapsed / 100

DeltaRmbMoveY = (DestinationY - RMB(RmbID).GetPosition.Y) * TV.TimeElapsed / 100

DeltaRmbMoveZ = (DestinationZ - RmbPosZ(RmbID)) * TV.TimeElapsed / 100



RmbPosX(RmbID) = RmbPosX(RmbID) + DeltaRmbMoveX

RmbPosZ(RmbID) = RmbPosZ(RmbID) + DeltaRmbMoveZ


RMB(RmbID).SetPosition RmbPosX(RmbID), RMB(RmbID).GetPosition.Y + DeltaRmbMoveY, RmbPosZ(RmbID)

End Sub









'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
Public Sub f重置跑道()


Randomize


For i = 1 To 15


         If camX - 1000 > NRWay(i).GetPosition.X Then '已经越过模型了
         
         
         NRWPosZ(i) = 300 * Int(5 * Rnd(1) + 1)
         
         
         NRWPosY(i) = -300 * Rnd(1) + 300
         
         
         NRWay(i).SetPosition camX + 6000, NRWPosY(i), NRWPosZ(i)
         
         
         End If
         
         
Next i


For a = 1 To 30


If Asteroid(a).GetPosition.Z < -200 Or Asteroid(a).GetPosition.Z > 2000 Or _
Asteroid(a).GetPosition.Y < -500 Or Asteroid(a).GetPosition.Y > 1500 Then '超出个框的话就复位


Asteroid(a).SetPosition Asteroid(a).GetPosition.X, 700, 700 + 300 * Rnd(1)


End If
'AbsoluteRotateX
AstMP(a).AbsRX = AstMP(a).AbsRX + AstMP(a).RX * TimePassSpeed


AstMP(a).AbsRY = AstMP(a).AbsRY + AstMP(a).RY * TimePassSpeed


AstMP(a).AbsRZ = AstMP(a).AbsRZ + AstMP(a).RZ * TimePassSpeed


Asteroid(a).SetRotation AstMP(a).AbsRX, AstMP(a).AbsRY, AstMP(a).AbsRZ '绝对的旋转角度

'陨石移动
Asteroid(a).MoveRelative _
AstMP(a).MX * TV.TimeElapsed * TimePassSpeed, _
AstMP(a).MY * TV.TimeElapsed * TimePassSpeed, _
AstMP(a).MZ * TV.TimeElapsed * TimePassSpeed


If camX - 500 > Asteroid(a).GetPosition.X Then '已经越过模型了


       Asteroid(a).SetPosition camX + 4500, Asteroid(a).GetPosition.Y, Asteroid(a).GetPosition.Z


End If


Next a

'重置money///////////////////////////////////////
If SE.EventActivated = True And SE.EventID = SpecialEvent_NoGravityArea Then
重置金币 (ResetMode_Random)
Else
重置金币 (ResetMode_Normal)
End If
End Sub











'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
Public Function 二进制加密(InputValue As Double) As Double
'在每8个二进制中，第N个就乘N
EnCryptionFlag = EnCryptionFlag + 1

If EnCryptionFlag > 8 Then EnCryptionFlag = 1

二进制加密 = InputValue * EnCryptionFlag

End Function


Public Function 二进制解密(InputValue As Double) As Double
'在每8个二进制中，第N个就乘N
DeCryptionFlag = DeCryptionFlag + 1

If DeCryptionFlag > 8 Then EnCryptionFlag = 1

二进制解密 = InputValue / DeCryptionFlag

End Function



Public Sub 数据存档(DataSaveMode As DataInputSaveMode)

EnCryptionFlag = 0 '加密
DeCryptionFlag = 0 '解密


'////////////////////////////////////////////////////
If DataSaveMode = DataType_SettingData Then   '存设置数据

Open App.Path & "\Save\Settings.sav" For Binary As #1
Put #1, 1, 二进制加密(Val(GameSettings.MouseSensitivity))

If GameSettings.IsSoundOn = True Then
Put #1, 9, 1
Else
Put #1, 9, 0
End If

Close #1

End If
'//////////////////////////////////////////////////////
If DataSaveMode = DataType_PlayerData Then '存玩家数据

Open App.Path & "\Save\Player.sav" For Binary As #2
Put #2, 1, 二进制加密(Val(TotalCash))
Put #2, 9, 二进制加密(Val(GameTools.Eject_TheNumberPlayerHas))
Put #2, 17, 二进制加密(Val(GameTools.MoneySave_TheNumberPlayerHas))
Put #2, 25, 二进制加密(Val(GameTools.HP_InShop_TheNumberPlayerHas))
Put #2, 33, 二进制加密(Val(HighScore))
Close #2

End If

End Sub




Public Sub 数据读取() '(InputMode As DataInputSaveMode)
'If InputMode = DataType_SettingData Then '加载设置
Dim gd As Byte

       Open App.Path & "\Save\Settings.sav" For Binary As #3
       Dim GetDataSpace As Double  '要用double类
       Get #3, 1, GetDataSpace
       GameSettings.MouseSensitivity = Val(GetDataSpace)
       Get #3, 9, gd '一字节
       
       If gd = 1 Then '布尔 在储存的时候true是1 false是0
       GameSettings.IsSoundOn = True
       Else
       GameSettings.IsSoundOn = False
       End If

       Close #3
'End If



'If InputMode = DataType_PlayerData Then '加载玩家数据
       Open App.Path & "\Save\Player.sav" For Binary As #4
       Dim GetDataSpace2 As Double  '要用double类
       'player那里存了4个double
       Get #4, 1, GetDataSpace2
       TotalCash = 二进制解密(Val(GetDataSpace2))
       Get #4, 9, GetDataSpace2
       GameTools.Eject_TheNumberPlayerHas = 二进制解密(Val(GetDataSpace2))
       Get #4, 17, GetDataSpace2
       GameTools.MoneySave_TheNumberPlayerHas = 二进制解密(Val(GetDataSpace2))
       Get #4, 25, GetDataSpace2
       GameTools.HP_InShop_TheNumberPlayerHas = 二进制解密(Val(GetDataSpace2))
       Get #4, 33, GetDataSpace2
       HighScore = 二进制解密(Val(GetDataSpace2))
       Close #4
'End If

End Sub

