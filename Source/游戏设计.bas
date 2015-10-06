Attribute VB_Name = "游戏设计"

Public Sub UI按钮判断(ButtonId As Integer)
With Button(ButtonId)

If MouseX > .LeftTop_X1 And MouseX < .LeftTop_X1 + .Width And _
MouseY > .LeftTop_Y1 And MouseY < .LeftTop_Y1 + .Height Then '在个矩形里面

.MouseIsOn = True

    '鼠标左键单击 时间包含在碰到按钮这个事件中
        If inputE.IsMouseButtonPressed(0) = True Then
    
        .MouseHasClick = True
    
        Else
    
        .MouseHasClick = False
    
        End If

Else

.MouseIsOn = False

End If


'//////不是傻逼= =是方便理解点 其实可以在上面的IF直接修改按钮状态
If .MouseIsOn = False Then .MouseStatus = Button_MouseNotOn

If .MouseIsOn = True Then .MouseStatus = Button_MouseOn

If .MouseIsOn = True And .MouseHasClick = True Then .MouseStatus = Button_IsClicked

'循环还没完 到了下面的next才是处理完一个按钮

'按钮要直接用SCR画 不调用
Select Case .MouseStatus
'鼠标没指着 BUTTON1
Case Button_MouseNotOn

SCR.Draw_Texture GetTex("Button1"), .LeftTop_X1, .LeftTop_Y1, .LeftTop_X1 + .Width, .LeftTop_Y1 + .Height

'鼠标指着
Case Button_MouseOn

SCR.Draw_Texture GetTex("Button2"), .LeftTop_X1, .LeftTop_Y1, .LeftTop_X1 + .Width, .LeftTop_Y1 + .Height

'按了下去
Case Button_IsClicked
'图的问题 BUTTON3的外发光大一点 微调对好位
SCR.Draw_Texture GetTex("Button3"), .LeftTop_X1 - 10, .LeftTop_Y1 - 15, .LeftTop_X1 + .Width + 20, .LeftTop_Y1 + .Height + 20

End Select

End With

End Sub
















Public Sub 绘图2D()
'获取鼠标坐标 PLAYLEVEL几都很重要的
inputE.GetAbsMouseState MouseX, MouseY

Select Case Playlevel

Case 1
'////////////背景
SCR.Draw_Texture GetTex("MenuBG"), 0, 0, FormCenterX * 2, FormCenterY * 2

'////////////////SPACERUN标志
SCR.Settings_SetAlphaBlending True, TV_BLEND_ADD

SCR.Draw_Texture GetTex("MenuTitle"), 500, 20, 950, 250

SCR.Settings_SetAlphaBlending False, TV_BLEND_ALPHA




'///////////////按钮

'先设为add模式
SCR.Settings_SetAlphaBlending True, TV_BLEND_ADD

For a = 1 To 4
'菜单的按钮
UI按钮判断 (a)

Next a


'按钮标题
SCRText.NormalFont_DrawText "开始游戏", Button(1).LeftTop_X1 + 20, Button(1).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
SCRText.NormalFont_DrawText "商    店", Button(2).LeftTop_X1 + 25, Button(2).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
SCRText.NormalFont_DrawText "设    置", Button(3).LeftTop_X1 + 25, Button(3).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
SCRText.NormalFont_DrawText "退    出", Button(4).LeftTop_X1 + 25, Button(4).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
'设回ALPHA模式
SCR.Settings_SetAlphaBlending False, TV_BLEND_ALPHA
'//////////////////微博
SCRText.NormalFont_DrawText "新浪微博:@鸡哥表示名字不会再爆格-爱执信", 20, 20, RGBA(1, 1, 1, 1), 3

















Case 2
    '///////////////特殊事件 GOLD BONUS提示
    
    
For a = 1 To 20
'透明度大于0 '这里处理淡出的文字
If ScrTextAlpha(a).Alpha > 0 Then

Select Case a

Case Text_小金块
SCRText.NormalFont_DrawText "小金块 +" & CStr(Gold_BonusCash), FormCenterX - 50, 50, RGBA(1, 1, 0.3, GetTextAlpha(Text_小金块)), 1

Case Text_人民币50 '50 X 加倍money
SCRText.NormalFont_DrawText "人民币 +" & CStr(50 * MoneyMultiplier), FormCenterX - 80, 50, RGBA(0.3, 1, 0.3, GetTextAlpha(Text_人民币50)), 1

Case Text_人民币1000
SCRText.NormalFont_DrawText "1000块 ！", FormCenterX - 80, 50, RGBA(0.6, 1, 0.6, GetTextAlpha(Text_人民币1000)), 1

Case Text_无重力模式
SCRText.NormalFont_DrawText "无重力模式！小心激光！", FormCenterX - 400, FormCenterY - 40, RGBA(0.5, 0.5, 1, GetTextAlpha(Text_无重力模式)), 2

Case Text_金块奖励模式
SCRText.NormalFont_DrawText "小金块奖励模式！", FormCenterX - 200, FormCenterY - 40, RGBA(1, 1, 0.3, GetTextAlpha(Text_金块奖励模式)), 2

Case text_撞到陨石
SCRText.NormalFont_DrawText "撞石头略疼 HP-", FormCenterX - 80, 50, RGBA(1, 0.3, 0.3, GetTextAlpha(text_撞到陨石)), 1

Case text_摔着了
SCRText.NormalFont_DrawText "摔得好疼 HP-", FormCenterX - 80, 50, RGBA(1, 0.3, 0.3, GetTextAlpha(text_摔着了)), 1

Case text_烧着了
SCRText.NormalFont_DrawText "要烧熟了 HP-", FormCenterX - 80, 50, RGBA(1, 0.3, 0.3, GetTextAlpha(text_烧着了)), 1

Case text_小心陨石
SCRText.NormalFont_DrawText "小心陨石！", FormCenterX - 160, FormCenterY - 60, RGBA(1, 0.3, 0.3, GetTextAlpha(text_小心陨石)), 2

Case text_变速隧道出现
SCRText.NormalFont_DrawText "变速通道出现！", FormCenterX - 200, FormCenterY - 60, RGBA(1, 0.3, 0.3, GetTextAlpha(text_变速隧道出现)), 2

Case text_石块会动
SCRText.NormalFont_DrawText "石跑道间歇性移动....", FormCenterX - 250, FormCenterY - 60, RGBA(1, 0.3, 0.3, GetTextAlpha(text_石块会动)), 2

Case text_拿到磁铁
SCRText.NormalFont_DrawText "道具磁铁", FormCenterX - 160, FormCenterY - 60, RGBA(0.3, 0.5, 0.1, GetTextAlpha(text_拿到磁铁)), 2

Case text_存钱
SCRText.NormalFont_DrawText "RMB存入账户 +" & UnSavedCashInGame, FormCenterX - 250, FormCenterY - 60, RGBA(0.6, 1, 0.6, GetTextAlpha(text_存钱)), 2

Case text_满血
SCRText.NormalFont_DrawText "补满血！", FormCenterX - 100, FormCenterY - 60, RGBA(1, 0.5, 0.3, GetTextAlpha(text_满血)), 2

Case text_道具未冷却
SCRText.NormalFont_DrawText "道具未冷却", FormCenterX - 80, 80, RGBA(0.3, 0.5, 1, GetTextAlpha(text_道具未冷却)), 1

Case text_开启自动存钱系统
SCRText.NormalFont_DrawText "开启自动存钱系统", FormCenterX - 120, 80, RGBA(1, 1, 1, GetTextAlpha(text_开启自动存钱系统)), 1

Case text_HP已满
SCRText.NormalFont_DrawText "HP已满", FormCenterX - 80, 80, RGBA(0.3, 0.5, 1, GetTextAlpha(text_HP已满)), 1

Case text_滞空时间加成
SCRText.NormalFont_DrawText "滞空时间加成 +" & StayInAirBonus, FormCenterX - 80, 80, RGBA(0.3, 0.5, 1, GetTextAlpha(text_滞空时间加成)), 1

Case text_时间流逝速度
SCRText.NormalFont_DrawText "时间流逝速度 X" & TimePassSpeed, FormCenterX - 200, FormCenterY - 30, RGBA(0.3, 0.5, 1, GetTextAlpha(text_时间流逝速度)), 2

End Select

End If 'IF ALPHA>0
'每个文字的透明度按照预设的速度递减
ScrTextAlpha(a).Alpha = ScrTextAlpha(a).Alpha - ScrTextAlpha(a).FadeOutSpeed * TV.TimeElapsed
Next a


'/////////////路程表盘
SCR.Settings_SetAlphaBlending True, TV_BLEND_ADD

SCR.Draw_Texture GetTex("SpeedDial"), 0, 150, 200, 300, RGBA(1, 1, 1, 0.5)

SCRText.NormalFont_DrawText CStr(Int(Scam / 10)) & " KM", 20, 250, RGBA(1, 1, 1, 1), 3

SCR.Settings_SetAlphaBlending False, TV_BLEND_ALPHA


    '///////////////////////////////HP
'画个心形
'血量越少 心形越小
SCR.Draw_Texture GetTex("heart"), 0, FormHeight - Int(HP) * 2, Int(HP) * 2, FormHeight, RGBA(1, 1, 1, 1)

'HP 血量
SCRText.NormalFont_DrawText CStr(Int(HP)), 10, FormHeight - 90, RGBA(0.3, 0.5, 1, 1), 2


'/////////////////////Money
SCR.Draw_Texture GetTex("RMB50"), 10, 10, 100, 60

SCRText.NormalFont_DrawText "背包里的钱￥ " & CStr(UnSavedCashInGame), 20, 40, RGBA(0, 0.5, 0.5, 1), 3

SCRText.NormalFont_DrawText "总账户的钱￥ " & CStr(TotalCash), 20, 80, RGBA(0, 0.5, 0.5, 1), 3

SCRText.NormalFont_DrawText "倍数 X", 20, 120, RGBA(1, 1, 1, 0.7), 3

SCRText.NormalFont_DrawText CStr(MoneyMultiplier), 150, 100, RGBA(1, 1, 1, 0.7), 2 '大号字

'ADD模式会影响淡入淡出

'///////////////////////有些模型的2D标明
'距离大于1000
If GetDistance(camX, camY, camZ, MoneySaveRunway.GetPosition.X, MoneySaveRunway.GetPosition.Y, MoneySaveRunway.GetPosition.Z) > 3000 Then
SCR.Math_3DPointTo2D MoneySaveRunway.GetPosition, MoneySave2DPointX, MoneySave2DPointY, True
SCRText.NormalFont_DrawText "存钱跑道", MoneySave2DPointX, MoneySave2DPointY, RGBA(1, 1, 1, 0.5), 4
End If


If SE.EventActivated = True Then
    '加减速隧道
    If SE.EventID = SpecialEvent_SpeedTunnel Then
    SCR.Math_3DPointTo2D SpeedUpTunnel.GetPosition, SpeedUpDPointX, SpeedUp2DPointY, False
    SCRText.NormalFont_DrawText "加速跑道", SpeedUpDPointX, SpeedUp2DPointY, RGBA(1, 1, 1, 0.5), 4

    SCR.Math_3DPointTo2D SpeedUpTunnel.GetPosition, SpeedDownDPointX, SpeedDown2DPointY, False
    SCRText.NormalFont_DrawText "减速速跑道", SpeedDownDPointX, SpeedDown2DPointY, RGBA(1, 1, 1, 0.5), 4
    End If
    

    '提示陨石下落点
    If SE.EventID = SpecialEvent_RockFall Then
    For i = 1 To 30
        '有一定高度再画点
        If FR(i).Mesh.GetPosition.Y > 150 Then
        SCR.Math_3DPointTo2D FR(i).Mesh.GetPosition, Fr2DX(i), Fr2DY(i), True
        SCR.Draw_FilledCircle Fr2DX(i), Fr2DY(i), 3, 18, RGBA(1, 0, 0, 1)
        End If
    Next i
    End If

End If

'///////游戏分数 另外在拿钱 拿道具之类的也加分
GameScore = GameScore + Int(Vcam * TV.TimeElapsed / 25)

SCRText.NormalFont_DrawText "分数", FormWidth - 50, 70, RGBA(1, 1, 1, 1), 3

SCRText.NormalFont_DrawText Int(CStr(GameScore)), FormWidth - 200, 20, RGBA(1, 1, 1, 0.8), 1

'///////////////////////////道具拥有数
SCR.Settings_SetAlphaBlending True, TV_BLEND_ADDALPHA
'心形
SCR.Draw_Texture GetTex("heart"), 250, FormHeight - 70, 300, FormHeight - 20
SCRText.NormalFont_DrawText CStr(GameTools.HP_InShop_TheNumberPlayerHas), 310, FormHeight - 70, RGBA(1, 1, 1, 1), 1
'向上的箭头
SCR.Draw_Texture GetTex("EjectionArrow"), 350, FormHeight - 70, 400, FormHeight - 20
SCRText.NormalFont_DrawText CStr(GameTools.Eject_TheNumberPlayerHas), 410, FormHeight - 70, RGBA(1, 1, 1, 1), 1
'自动存钱
SCR.Settings_SetAlphaBlending True, TV_BLEND_ALPHA
SCR.Draw_Texture GetTex("RMB50"), 450, FormHeight - 70, 500, FormHeight - 20
SCRText.NormalFont_DrawText CStr(GameTools.MoneySave_TheNumberPlayerHas), 510, FormHeight - 70, RGBA(1, 1, 1, 1), 1
'moneysave的状态
If GameTools.MoneySave_IsUsing = True Then
SCRText.NormalFont_DrawText "已开启", 550, FormHeight - 70, RGBA(0.3, 0.5, 1, 1), 3
Else
SCRText.NormalFont_DrawText "已关闭", 550, FormHeight - 70, RGBA(1, 0, 0, 1), 1
End If



'//////////////////////////滞空时间加成

If StayInAirTime > 3000 And SE.EventID <> SpecialEvent_NoGravityArea And GameTools.Eject_IsUsingSystem = False Then
'时间越长加成越多
StayInAirBonus = Int((StayInAirTime - 3000) * TimePassSpeed)

SetTextAlpha text_滞空时间加成, 1
'SCRText.NormalFont_DrawText "滞空时间加成+" & CStr(StayInAirBonus), FormCenterX - 120, 100, RGBA(0.3, 0.5, 1, 0.7), 1
End If


If scene.Collision(Vector(camX, camY, camZ), Vector(camX, camY - 100, camZ)) = True Then  '着陆了
StayInAirTime = 0
       If StayInAirTime > 1500 Then
       UnSavedCashInGame = UnSavedCashInGame + (StayInAirTime - 1500)
       GameScore = GameScore + (StayInAirTime - 1500) * 5
       End If
Else
'滞空时间+
StayInAirTime = StayInAirTime + TV.TimeElapsed
End If


'////////////////////////EjectionSystem
SCR.Settings_SetAlphaBlending True, TV_BLEND_MULTIPLY
If GameTools.Eject_HasStartTime > 500 And GameTools.Eject_HasStartTime < 5000 Then
SCR.Draw_FilledBox 0, 0, FormWidth, FormHeight, RGBA(1, 0, 0, 1)
End If
'If TimePassSpeed < 1 Then SCR.Draw_FilledBox 0, 0, FormWidth, FormHeight, RGBA(0, 1, 1, 1)
SCR.Settings_SetAlphaBlending True, TV_BLEND_ALPHA










'//////////////////////////////////
Case 3 '死掉


SCR.Settings_SetAlphaBlending True, TV_BLEND_ADD

'顶部标题
SCR.Draw_Texture GetTex("MenuTitle"), FormCenterX - 200, 50, FormCenterX + 200, 250

'死后的评分屏 只用3个按钮 号码是5到7
For a = 5 To 7

Button(a).LeftTop_X1 = Int(FormCenterX) - 1650 + a * 250 '横着排列

'菜单的按钮
UI按钮判断 (a)

'按钮标题 按钮用add模式 不要影响文字
If a = 5 Then SCRText.NormalFont_DrawText "重新开始", Button(5).LeftTop_X1 + 25, Button(5).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
If a = 6 Then SCRText.NormalFont_DrawText "商    店", Button(6).LeftTop_X1 + 25, Button(6).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
If a = 7 Then SCRText.NormalFont_DrawText "返回菜单", Button(7).LeftTop_X1 + 25, Button(7).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1


'评分屏
DieMenuCountTime = DieMenuCountTime + TV.TimeElapsed '计时器


If DieMenuCountTime > 500 Then
        If DieMenuCountTime <= 600 Then GE.Flash 1, 1, 1, 300
SCRText.NormalFont_DrawText "最高分： " & Int(CStr(HighScore)), 400, 230, RGBA(1, 1, 1, 1), 1
SCRText.NormalFont_DrawText "总分数： " & Int(CStr(GameScore)), 400, 280, RGBA(1, 1, 1, 1), 1
End If

If DieMenuCountTime > 1000 Then
        If DieMenuCountTime <= 1100 Then GE.Flash 1, 1, 1, 300
SCRText.NormalFont_DrawText "走过的路程： " & Int(Scam / 10) + 20 & " KM", 400, 330, RGBA(1, 1, 1, 1), 1
End If

If DieMenuCountTime > 1500 Then
        If DieMenuCountTime <= 1600 Then GE.Flash 1, 1, 1, 300
SCRText.NormalFont_DrawText "账户余额：￥ " & TotalCash, 400, 380, RGBA(1, 1, 1, 1), 1
End If

If DieMenuCountTime > 2000 Then
        If DieMenuCountTime <= 2100 Then GE.Flash 1, 1, 1, 300
SCRText.NormalFont_DrawText "还有 ￥" & UnSavedCashInGame & "被浪费了", 400, 430, RGBA(1, 1, 1, 1), 1
End If

Next a


SCR.Settings_SetAlphaBlending True, TV_BLEND_ALPHA












'///////////////////////////////////////////
'//////////////////////////////////////////商店
Case 4
'RMB50
SCR.Draw_Texture GetTex("RMB50"), 150, 350, 230, 400
'换模式
SCR.Settings_SetAlphaBlending True, TV_BLEND_ADD
'标题
SCR.Draw_Texture GetTex("MenuTitle"), 20, 50, 350, 250
SCRText.NormalFont_DrawText "商 店", 100, 250, RGBA(1, 0.8, 0.6, 1), 2
SCRText.NormalFont_DrawText "账户余额： ￥" & TotalCash, 80, 450, RGBA(0.6, 1, 0.6, 1), 3


For a = 8 To 11
'菜单的按钮
UI按钮判断 (a)
If a = 8 Then
SCRText.NormalFont_DrawText "急 救 箱", Button(a).LeftTop_X1 + 25, Button(a).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
SCRText.NormalFont_DrawText "￥3000", Button(a).LeftTop_X1 + 250, Button(a).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
SCRText.NormalFont_DrawText "拥有数：" & GameTools.HP_InShop_TheNumberPlayerHas, Button(a).LeftTop_X1 - 150, Button(a).LeftTop_Y1 + 20, RGBA(0.6, 1, 0.6, 1), 3

End If

If a = 9 Then
SCRText.NormalFont_DrawText "弹射系统", Button(a).LeftTop_X1 + 25, Button(a).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
SCRText.NormalFont_DrawText "￥6000", Button(a).LeftTop_X1 + 250, Button(a).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
SCRText.NormalFont_DrawText "拥有数：" & GameTools.Eject_TheNumberPlayerHas, Button(a).LeftTop_X1 - 150, Button(a).LeftTop_Y1 + 20, RGBA(0.6, 1, 0.6, 1), 3
End If

If a = 10 Then
SCRText.NormalFont_DrawText "自动存钱", Button(a).LeftTop_X1 + 25, Button(a).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
SCRText.NormalFont_DrawText "￥4000", Button(a).LeftTop_X1 + 250, Button(a).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
SCRText.NormalFont_DrawText "拥有数：" & GameTools.MoneySave_TheNumberPlayerHas, Button(a).LeftTop_X1 - 150, Button(a).LeftTop_Y1 + 20, RGBA(0.6, 1, 0.6, 1), 3
End If

If a = 11 Then
SCRText.NormalFont_DrawText "返回菜单", Button(a).LeftTop_X1 + 25, Button(a).LeftTop_Y1 + 20, RGBA(1, 0.3, 0.5, 1), 1
End If



'商品描述
If Button(a).MouseIsOn = True Then
    Select Case a
    Case 8
    SCRText.NormalFont_DrawText "使用一个就可以补满血，伤了血之后才能用。", 500, 500, RGBA(1, 1, 1, 1), 3
    Case 9
    SCRText.NormalFont_DrawText "弹射系统，超短时间积蓄能量，喷发，向上加速，", 500, 500, RGBA(1, 1, 1, 1), 3
    SCRText.NormalFont_DrawText "坠落致死之前都可以拿来救命。", 500, 530, RGBA(1, 1, 1, 1), 3
    Case 10
    SCRText.NormalFont_DrawText "在本关每隔7秒直接帮你把钱存入账户。（不然就要在存钱跑道上存）", 500, 500, RGBA(1, 1, 1, 1), 3
    Case 11
    SCRText.NormalFont_DrawText "返回主菜单。", 500, 500, RGBA(1, 1, 1, 1), 3
    End Select
End If



Next a









'///////////////////////////////设置
'///////////////////////////////设置
'///////////////////////////////设置
Case 5

SCR.Settings_SetAlphaBlending True, TV_BLEND_ADD
For a = 12 To 16
UI按钮判断 (a)
Next a

'鼠标灵敏度
SCRText.NormalFont_DrawText "-", Button(12).LeftTop_X1 + 10, Button(12).LeftTop_Y1, RGBA(0, 0, 0, 1), 2
SCRText.NormalFont_DrawText "+", Button(13).LeftTop_X1 + 10, Button(13).LeftTop_Y1, RGBA(0, 0, 0, 1), 2
SCRText.NormalFont_DrawText CStr(Round(GameSettings.MouseSensitivity, 1)), 500, 200, RGBA(1, 1, 1, 1), 2
SCRText.NormalFont_DrawText "鼠标灵敏度", 470, 320, RGBA(1, 1, 1, 1), 1

'音效状态
SCRText.NormalFont_DrawText "开", Button(14).LeftTop_X1 + 10, Button(14).LeftTop_Y1 + 20, RGBA(1, 1, 1, 1), 1
SCRText.NormalFont_DrawText "关", Button(15).LeftTop_X1 + 10, Button(15).LeftTop_Y1 + 20, RGBA(1, 1, 1, 1), 1

If GameSettings.IsSoundOn = True Then
SCRText.NormalFont_DrawText "音效状态 开", 430, 500, RGBA(1, 0.6, 0.6, 1), 1
Else
SCRText.NormalFont_DrawText "音效状态 关", 430, 500, RGBA(1, 0.6, 0.6, 1), 1
End If

SCRText.NormalFont_DrawText "返           回", Button(16).LeftTop_X1 + 50, Button(16).LeftTop_Y1 + 40, RGBA(1, 0.3, 0.6, 1), 1




'///////////////////////////////////////////暂停菜单
'///////////////////////////////////////////暂停菜单
'///////////////////////////////////////////暂停菜单
'///////////////////////////////////////////暂停菜单
Case 6
SCRText.NormalFont_DrawText "暂停菜单", 500, 50, RGBA(1, 0.5, 0.5, 1), 2

SCRText.NormalFont_DrawText _
"继       续", _
Button(17).LeftTop_X1 + 10, _
Button(17).LeftTop_Y1 + 10, _
RGBA(1, 1, 0.5, 1), 1

SCRText.NormalFont_DrawText _
"设       置", _
Button(18).LeftTop_X1 + 10, _
Button(18).LeftTop_Y1 + 10, _
RGBA(1, 1, 0.5, 1), 1


SCRText.NormalFont_DrawText _
"返回主菜单", _
Button(19).LeftTop_X1 + 10, _
Button(19).LeftTop_Y1 + 10, _
RGBA(1, 1, 0.5, 1), 1



End Select





SCR.Settings_SetAlphaBlending False, TV_BLEND_ALPHA





'/////////截屏的screentextalpha 所有playlevel都适用
If ScrTextAlpha(20).Alpha > 0 Then
SCRText.NormalFont_DrawText "已截屏", FormCenterX - 50, FormCenterY - 20, RGBA(1, 1, 1, GetTextAlpha(text_已截屏)), 1
End If
ScrTextAlpha(20).Alpha = ScrTextAlpha(20).Alpha - ScrTextAlpha(20).FadeOutSpeed * TV.TimeElapsed





End Sub















'////////////////////////////////////////////////////////////////////////////////////////////////////////////////
'////////////////////////////////////////////////////////////////////////////////////////////////////////////////
'////////////////////////////////////////////////////////////////////////////////////////////////////////////////
'////////////////////////////////////////////////////////////////////////////////////////////////////////////////
'////////////////////////////////////////////////////////////////////////////////////////////////////////////////
'////////////////////////////////////////////////////////////////////////////////////////////////////////////////
Public Sub 特殊事件()


特殊事件初始化


If SE.EventActivated = True Then '特殊事件被激活


    Select Case SE.EventID
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Case SpecialEvent_NoGravityArea  '1
    
    
    If camZ > 500 And camZ < 1500 And camY < 1000 And camY > -500 Then '不能离开区域啊
    
    '把正常的跳跃次数归零
    HasJumped = 0
    
    
    CanPressW = True
    
    
    NoGravityActivated = True
    
    
    Else
    
    
    NoGravityActivated = False
    
    
    End If
    
    
    
    
    
    '旋转
    
    '反正RMB都一直转的了 旋转的变量 干脆一起用吧 = =
    NGALaser.SetRotation RMBRotateY * 0.1, 0, 0
    
    
    
    
    If VY > 0 Then '用IF亲切点啊 Y向的减速度
    VY = VY - TV.TimeElapsed ^ 2 / 6400
        If VY < 0 Then VY = 0
    End If
    
    If VY < 0 Then
    VY = VY + TV.TimeElapsed ^ 2 / 6400
        If VY > 0 Then VY = 0
    End If
    
    
    
    
    
    If camX > NoGravityArea.GetGlobalPosition.X + 6000 Then  '——————————————————————事件结束
    
    NoGravityActivated = False
    
    SE.EventActivated = False
    
    SE.EventCountTime = 0
    
    SE.EventReBoostTime = 20000 + 10000 * Rnd(1)
    
    End If
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
        '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    
    Case SpecialEvent_RockFall '2
    
    
     ShockActivated = True
    
    
    f震屏 5000, 8 '用之前先激活
    
    
    For r = 1 To 30
    
    '直落
    'FR(r).X = FR(r).X - TV.TimeElapsed    '以同样的方向和速度降落噜 懒得弄不同的啦
    
    
    FR(r).Y = FR(r).Y - 2 * TV.TimeElapsed
    
    
    FR(r).Mesh.SetPosition FR(r).X, FR(r).Y, FR(r).StartZ
    
    
    
            For i = 1 To 15
            
            '落石爆炸
            If NRWay(i).Collision(Vector(FR(r).X - 200, FR(r).Y - 200, FR(r).Z - 200), Vector(FR(r).X + 200, FR(r).Y + 200, FR(r).Z + 200), TV_TESTTYPE_ACCURATETESTING) = True Or FR(r).Y < -3000 Then
        '上面那一长串 象征性地给了落石的大小 记录了碰撞的坐标
            
            If GameSettings.IsSoundOn = True Then GameSound.Explode(r).Play
            
        
            ExplosionParticle(r).SetGlobalPosition FR(r).X, FR(r).Y - 200, FR(r).Z
            
            
            ExplosionParticle(r).Update
            
                '爆炸那一刻伤血
                    If GetDistance(camX, camY, camZ, FR(r).X, FR(r).Y, FR(r).Z) < 50 Then
                
                '随便伤点血吧
                    HP = HP - GetDistance(camX, camY, camZ, FR(r).X, FR(r).Y, FR(r).Z) \ 3
                
                '闪下红
                    GE.Flash 1, 0, 0, 200
                
                
                    End If
            
            '可以爆炸了
            ExpAnim(r).Activated = True
            
        
            FRHasFallNumber = FRHasFallNumber + 1
        
            '把炸了落石扔开
            FR(r).X = 0
            
            
            FR(r).Y = 0
            
            
            FR(r).Z = 0
        
        
            FR(r).Mesh.SetPosition 0, 0, 0


            End If
            
            Next i
    
            '碰到爆炸火焰也伤血 不然就太简单了 = = 距离小于150的话 吗的代码太长了
            If GetDistance(camX, camY, camZ, ExplosionParticle(r).GetGlobalPosition.X, ExplosionParticle(r).GetGlobalPosition.Y, ExplosionParticle(r).GetGlobalPosition.Z) < 300 Then
                
                
            SetTextAlpha text_烧着了, 1
                
                
            HP = HP - 0.03 * TV.TimeElapsed
                
                
            GE.Flash 1, 0, 0, 200
                
                
            End If
    Next r
    
    
    '掉够30个陨石了
    If FRHasFallNumber >= 30 Then '——————————————————————事件结束
    
    SE.EventActivated = False
    
    SE.EventCountTime = 0
    
    SE.EventReBoostTime = 20000 + 10000 * Rnd(1)
    
    FRHasFallNumber = 0
    
    ShockActivated = False '震屏
    
    End If
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    
    
    
    Case SpecialEvent_SpeedTunnel '3
    
    
    ShockActivated = True
    
    
    f震屏 5000, 8 '用之前先激活
    
    
    If CamViewDegree < 160 Then CamViewDegree = CamViewDegree + 0.003 * TV.TimeElapsed


    If CamViewDegree > 157 Then CamViewDegree = 160 '在特殊事件3 加速隧道 会有这个镜头变换


    cam.SetViewFrustum CamViewDegree, 7000
    
    
    
    If SpeedUpTunnel.Collision(Vector(camX, camY, camZ), Vector(camX, camY + 1, camZ), TV_TESTTYPE_BOUNDINGBOX) = True Then
    Vcam = Vcam + 0.00025 * TV.TimeElapsed '加速
    
        If SpeedUpTunnel.Collision(Vector(camX, camY, camZ), Vector(camX, camY - 70, camZ), TV_TESTTYPE_ACCURATETESTING) = True Then
        VY = VY + 0.1 '身位高点
        End If
        
    End If
        
        
    If SpeedDownTunnel.Collision(Vector(camX, camY, camZ), Vector(camX, camY + 1, camZ), TV_TESTTYPE_BOUNDINGBOX) = True Then
    
        If Vcam > 10 Then '速度不能减太慢
        Vcam = Vcam - 0.0005 * TV.TimeElapsed
        End If
    
        If SpeedDownTunnel.Collision(Vector(camX, camY, camZ), Vector(camX, camY - 70, camZ), TV_TESTTYPE_ACCURATETESTING) = True Then
        VY = VY + 0.1
        End If
        
    End If


    If camX > SpeedTunnelX + 5000 Then '——————————————————————事件结束
    
    SE.EventActivated = False
    
    SE.EventCountTime = 0
    
    SE.EventReBoostTime = 20000 + 10000 * Rnd(1)
    
    ShockActivated = False
    
    End If
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    Case SpecialEvent_Gold  '4


    For a = 1 To 30
    
    '拿到小陨石
    If GetDistance(camX, camY, camZ, Asteroid(a).GetPosition.X, Asteroid(a).GetPosition.Y, Asteroid(a).GetPosition.Z) < 200 And _
    Asteroid(a).GetScale.X < 0.2 Then '因为SCALE的XYZ分量都一样 所以拿一个判断缩放大小就够了
    
    '闪一下黄的
    GE.Flash 1, 1, 0.3, 300
    
    
    Asteroid(a).MoveRelative -1000, 0, 0
    
    '按照小金块的大小 决定BONUS的钱数 搞成50的倍数吧
    Gold_BonusCash = Int(10000 * Asteroid(a).GetScale.X \ 50) * 50
    
    '加钱
    UnSavedCashInGame = UnSavedCashInGame + Gold_BonusCash

    '/////////////加分
    GameScore = GameScore + 5000 * MoneyMultiplier


    SetTextAlpha Text_小金块, 1
    
    
    End If
    Next
    
    
    SE_Gold_LastTime = SE_Gold_LastTime + TV.TimeElapsed
    
    
    
    
    
    If SE_Gold_LastTime > 15000 Then '事件结束
    
    GE.Flash 1, 1, 0.3, 1000
    
    For i = 1 To 30
    
    Asteroid(i).SetTexture GetTex("rock")
    
    Asteroid(i).SetMaterial GetMat("asteroid")
    
    Asteroid(i).SetLightingMode TV_LIGHTING_NORMAL
    
    Next i
    
    SE_Gold_LastTime = 0
    
    SE.EventActivated = False
    
    SE.EventCountTime = 0
    
    SE.EventReBoostTime = 20000 + 10000 * Rnd(1)
    
    ShockActivated = False
     
    End If
    
    
    
    
    
    
    
    
    
    
    
    
    
    
        '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    '///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Case 5
    
    RockHasMove_Time = RockHasMove_Time + TV.TimeElapsed

    For r = 1 To 15 'sin周期函数
    NRWay(r).SetPosition _
    NRWay(r).GetPosition.X, _
     NRWay(r).GetPosition.Y, _
    TmpNRWayPosZ(r) + 200 * (Sin(RockHasMove_Time * TimePassSpeed / MoveDifferenceConst(r)))
    Next r
    


    'If RockHasMove_Time > 9000 Then '1800毫秒
    'RockHasMove_Rounds = RockHasMove_Rounds + 1 '来回数加一
    'End If
    
    
    If RockHasMove_Time > 18000 Then    '已经搞了几个来回
    RockHasMove_Time = 0
    RockHasMove_Rounds = 0
    
    SE.EventActivated = False
    SE.EventCountTime = 0
    SE.EventReBoostTime = 20000 + 10000 * Rnd(1)
    End If
    
    
    
    End Select


End If


   ' WriteLog Time & Chr(13) & Chr(10) & "Activated" & SE.EventActivated
   ' WriteLog "Cam X:" & camX & " Y: " & camY
   ' WriteLog "Rock FR(1).X" & FR(1).X & " Z: " & FR(1).Z
   'WriteLog GetDistance(camX, camY, camZ, FR(1).X, FR(1).Y, FR(1).Z)

SE.EventCountTime = SE.EventCountTime + TV.TimeElapsed


End Sub










































'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
Public Sub 特殊事件初始化()

If SE.EventCountTime >= SE.EventReBoostTime And SE.EventActivated = False Then '时间到了事件触发 先初始化


Randomize


SE.EventReBoostTime = 20000 + 10000 * Rnd(1) '再充时间为20 + (0~10)秒


SE.EventCountTime = 0 '下一个事件计时器 归零


SE.EventActivated = True


SE.EventID = Int(5 * Rnd(1) + 1)



    Select Case SE.EventID '事件数据初始化 每次执行一次咯
    
    '///////////////////////////////////////1111111111111////////////////////////////////////////////////////////////////////////
    Case SpecialEvent_NoGravityArea  '1
    
    
    GE.Flash 0.5, 0.5, 1, 300
    
    
    SetTextAlpha Text_无重力模式, 1
    
    
    NoGravityArea.SetGlobalPosition camX + 7000, 300, 1000


    NGALaser.SetPosition camX + 4000, 400, 1000
    
    

    '//////////////////////////////////////222222222222222222/////////////////////////////////////////////////////////////////////
    Case SpecialEvent_RockFall '2
    
    
    SetTextAlpha text_小心陨石, 1
    
    
    For r = 1 To 30
    
    
    'FRExtraDistanceX(r) = 9000 * Rnd(1)
    
    '设置在camera前面
    FR(r).StartX = camX + 280 * Vcam + r * 50
    
    
    FR(r).StartY = 6000 + r * 500 '
    
    
    FR(r).StartZ = 1000 * Rnd(1) + 400 '500 + 500 * Rnd(1)
    
    
    FR(r).X = FR(r).StartX
    
    
    FR(r).Y = FR(r).StartY
    
    
    FR(r).Z = FR(r).StartZ
    
    
    FR(r).Mesh.SetPosition FR(r).StartX, FR(r).StartY, FR(r).StartZ
    
    
    ExplosionParticle(r).ResetAll
    
    
    Next r
    
    '//////////////////////////////////////////3333333333333333//////////////////////////////////////////////////////////////////////////////
    Case SpecialEvent_SpeedTunnel '3
    
    SetTextAlpha text_变速隧道出现, 1
    
    
    '移位啦 两个隧道 两个转动的环
    SpeedTunnelX = camX + 6000 '
    
    
    SpeedUpTunnel.SetPosition SpeedTunnelX, 350, 0
    
    '分别放两边 Z为0 和 2000
    SpeedDownTunnel.SetPosition SpeedTunnelX, 350, 2000
    
    
    SpeedTunnelRing(1).SetPosition SpeedTunnelX, 350, 0
    
    
    SpeedTunnelRing(2).SetPosition SpeedTunnelX, 350, 2000
    
    
    SpeedTunnelRing(1).PlayAnimation
    
    
    SpeedTunnelRing(2).PlayAnimation
    
    '粒子系统 箭头
    SpeedUpParticle.SetGlobalPosition SpeedTunnelRing(1).GetPosition.X, SpeedTunnelRing(1).GetPosition.Y, SpeedTunnelRing(1).GetPosition.Z
    
    
    SpeedDownParticle.SetGlobalPosition SpeedTunnelRing(2).GetPosition.X, SpeedTunnelRing(2).GetPosition.Y, SpeedTunnelRing(2).GetPosition.Z
    
    
    
    '///////////////////////////////////////////////444444444444444444//////////////////////////////////////////////////////////////////////////////////
    Case SpecialEvent_Gold '4


    GE.Flash 1, 1, 0.3
    
    
    SetTextAlpha Text_金块奖励模式, 1
    
    
    For i = 1 To 30
    
    Asteroid(i).SetLightingMode TV_LIGHTING_BUMPMAPPING_TANGENTSPACE
    
    Asteroid(i).SetTexture GetTex("Gold")
    
    Asteroid(i).SetMaterial GetMat("Solid")
    
    Next i
    
    
    
    '/////////////////////////////555555555556666666666666666666677777777777777777/////////////////////////////
    
    Case 5
    
    SetTextAlpha text_石块会动, 1
    
    RockHasMove_Time = 0 '石块开始移了多久
    
    RockHasMove_Rounds = 0 '石块移了多少个来回
    
    Randomize
    For a = 1 To 15
    TmpNRWayPosZ(a) = NRWay(a).GetPosition.Z
    MoveDifferenceConst(a) = 500 + 5000 * Rnd(1) 'sin(x / MoveDifference) 控制周期速度 （波长）
    Next a

    End Select
    
    
End If

End Sub

















'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
'//////////////////////////////////////////////////////////////////////////////////////////////////////////////
Public Sub 道具与存钱跑道()

With GameTools



'/////////////////////////////////////////////////////磁铁
'/////////////////////////////////////////////////////磁铁
'/////////////////////////////////////////////////////磁铁
'/////////////////////////////////////////////////////磁铁
.Magnet_Mesh.SetRotation 0, RMBRotateY * 2, 0 '磁铁跟着RMB一起旋转


'玩家拿到磁铁
If .Magnet_IsPlayerHas = False And _
    GetDistance(camX, camY, camZ, .Magnet_Mesh.GetPosition.X, .Magnet_Mesh.GetPosition.Y, .Magnet_Mesh.GetPosition.Z) < 200 Then
       '音阶音效
       If GameSettings.IsSoundOn = True Then GameSound.GetTools.Play

    SetTextAlpha text_拿到磁铁, 1

    GE.Flash 1, 1, 1, 300
    '玩家拥有了
    .Magnet_IsPlayerHas = True

    '放到远处
    .Magnet_Mesh.SetPosition camX + Vcam * 2000, 300, 1000

End If

'/////越过磁铁也移动
If camX - (.Magnet_Mesh.GetPosition.X) > 300 Then .Magnet_Mesh.SetPosition camX + Vcam * 3000, 300, 500 + 1000 * Rnd(1)





'拥有磁铁的时候
If .Magnet_IsPlayerHas = True Then

.Magnet_OnHandTime = .Magnet_OnHandTime + TV.TimeElapsed

For i = 1 To 70

    If GetDistance(camX, camY, camZ, RmbPosX(i), RMB(i).GetPosition.Y, RmbPosZ(i)) < 500 Then
    'MONEY被磁化
    RmbIsMagnetic(i) = True

    End If

    
    If RmbIsMagnetic(i) = True Then
    
    '重置金币 钱到范围内会拿到钱的
    MoveRMB Val(i), camX, camY, camZ
        
    End If

Next

End If





'够时间就没了磁铁
If .Magnet_OnHandTime > 10000 Then
.Magnet_IsPlayerHas = False
.Magnet_OnHandTime = 0
    For i = 1 To 70
    RmbIsMagnetic(i) = False
    Next
End If












'/////////////////////////////////////////////////////////////储存MONEY跑道
'/////////////////////////////////////////////////////////////储存MONEY跑道
'/////////////////////////////////////////////////////////////储存MONEY跑道
'/////////////////////////////////////////////////////////////储存MONEY跑道
If MoneySaveRunway.Collision(Vector(camX, camY, camZ), Vector(camX, camY - 100, camZ), TV_TESTTYPE_BOUNDINGBOX) = True Then

        If HaveSavedMoney = False Then SetTextAlpha text_存钱, 1

HaveSavedMoney = True

MoneyMultiplier = 1

End If




'可以存钱而且 淡出字已经写完
If HaveSavedMoney = True And GetTextAlpha(text_存钱) <= 0 Then
TotalCash = TotalCash + UnSavedCashInGame
UnSavedCashInGame = 0
End If



If MoneySaveRunway.GetPosition.X + 3000 < camX Then
    
    'mul不超5 ，然后这次见到跑道不存钱就加multiplier
    If MoneyMultiplier < 5 And HaveSavedMoney = False Then
    MoneyMultiplier = MoneyMultiplier + 1
    End If

HaveSavedMoney = False

MoneySaveRunway.SetPosition camX + Vcam * 1500, 350, 500 + 1000 * Rnd(1)

End If












'///////////////////////////////////////补血心形
'///////////////////////////////////////补血心形
'///////////////////////////////////////补血心形
'///////////////////////////////////////补血心形
.RecoverHP_Mesh.SetRotation 0, RMBRotateY * 2, 0


With GameTools.RecoverHP_Mesh
'拿到道具
If GetDistance(camX, camY, camZ, .GetPosition.X, .GetPosition.Y, .GetPosition.Z) < 200 Then
'音阶音效
If GameSettings.IsSoundOn = True Then GameSound.GetTools.Play

SetTextAlpha text_满血, 1

GE.Flash 1, 1, 1, 300

HP = 100

.SetPosition camX + Vcam * 3000, 300, 500 + 1000 * Rnd(1)

End If

End With 'Gametools.RecoverHP

End With 'Gametools






'////////////////////////////////////////改变流逝时间
'////////////////////////////////////////改变流逝时间
'////////////////////////////////////////改变流逝时间
'////////////////////////////////////////改变流逝时间
With GameTools.TimeSlow_Mesh
'道具旋转
.SetRotation 0, RMBRotateY * 2, 0
'拿到道具
If GetDistance(camX, camY, camZ, .GetPosition.X, .GetPosition.Y, .GetPosition.Z) < 200 Then
'音阶音效
If GameSettings.IsSoundOn = True Then GameSound.GetTools.Play

SetTextAlpha text_时间流逝速度, 1
'放慢时间
TimePassSpeed = 0.5
'闪
GE.Flash 1, 1, 1, 300

GameTools.TimeSlow_IsPlayerHas = True
'扔远处
.SetPosition camX + Vcam * 4000, 300, 500 + 1000 * Rnd(1)

End If

'经过CLOCK 没拿到也重置
If camX > .GetPosition.X + 500 Then .SetPosition camX + Vcam * 4000, 300, 500 + 1000 * Rnd(1)


'正在放慢镜的时候
If GameTools.TimeSlow_IsPlayerHas = True Then
'已拿道具计时器
GameTools.TimeSlow_OnHandTime = GameTools.TimeSlow_OnHandTime + TV.TimeElapsed
       If GameTools.TimeSlow_OnHandTime > 15000 Then
       GameTools.TimeSlow_OnHandTime = 0
       GameTools.TimeSlow_IsPlayerHas = False
       TimePassSpeed = 1
       End If
End If







'////////////////////////////////特殊按键与道具
'////////////////////////////////特殊按键与道具
'////////////////////////////////特殊按键与道具
'////////////////////////////////特殊按键与道具
KeyCountTime.Key_LeftCtrl_Time = KeyCountTime.Key_LeftCtrl_Time + TV.TimeElapsed 'LCTRL
KeyCountTime.Key_Space_Time = KeyCountTime.Key_Space_Time + TV.TimeElapsed '空格
KeyCountTime.Key_PrintScreen = KeyCountTime.Key_PrintScreen + TV.TimeElapsed '截屏

'////////////////////补血
If inputE.IsKeyPressed(TV_KEY_LEFTCONTROL) And _
GameTools.HP_InShop_TheNumberPlayerHas > 0 Then '按了ctrl，伤了血，拥有数大于零
       If HP = 100 Then SetTextAlpha text_HP已满, 1
       If HP < 100 Then
              If KeyCountTime.Key_LeftCtrl_Time > 5000 Then '可以按CTRL
       
              GE.Flash 1, 1, 1, 300
              
              '音阶音效
              If GameSettings.IsSoundOn = True Then GameSound.GetTools.Play
              
              
              SetTextAlpha text_满血, 1
       
              HP = 100
       
              GameTools.HP_InShop_TheNumberPlayerHas = GameTools.HP_InShop_TheNumberPlayerHas - 1
       
              '重置冷却时间
              KeyCountTime.Key_LeftCtrl_Time = 0
       
              数据存档 (DataType_PlayerData)
       
              Else '未冷却
       
              SetTextAlpha text_道具未冷却, 1
       
              End If
              
       End If
       
End If








'///////////////////////////////////////////////////弹射系统
If inputE.IsKeyPressed(TV_KEY_SPACE) And GameTools.Eject_IsUsingSystem = False And _
GameTools.Eject_TheNumberPlayerHas > 0 Then '按了space，没在用，拥有数大于零

              If KeyCountTime.Key_Space_Time > 10000 Then '可以按CTRL
              
              StayInAirTime = 0
              
              GameTools.Eject_IsUsingSystem = True
              
              GameTools.Eject_TheNumberPlayerHas = GameTools.Eject_TheNumberPlayerHas - 1
              
             '重置冷却时间
             KeyCountTime.Key_Space_Time = 0
             
              数据存档 (DataType_PlayerData)
              
             Else '未冷却
             
             SetTextAlpha text_道具未冷却, 1
             
             End If
             
End If

If GameTools.Eject_IsUsingSystem = True Then
GameTools.Eject_HasStartTime = GameTools.Eject_HasStartTime + TV.TimeElapsed
       Select Case GameTools.Eject_HasStartTime
       '淡出
       Case 0 To 200
       TimePassSpeed = 0.05
       GE.Flash 1, 1, 1, 500
       '慢镜
       Case 300 To 3500
       '把BGM弱化 强调心跳声
       GameBGM(GameBGMID).Volume = -2000
       If GameSettings.IsSoundOn = True Then GameSound.HeartBeat.Play
       TimePassSpeed = 0.05
       
       Case 3500 To 5000
       GameSound.Eject_Charge.Play
       
       '恢复
       Case Is > 5000
       If GameSettings.IsSoundOn = True Then GameSound.HeartBeat.Stop_
       GameSound.Eject_Charge.Stop_
       GameBGM(GameBGMID).Volume = -800
       '滞空时间归零
       StayInAirTime = 0
       '时间流逝速度
       TimePassSpeed = 1
       '有速度限制 3*TV.TIMEELAPSED
       VY = 1.5 * TV.TimeElapsed
       If GameSettings.IsSoundOn = True Then GameSound.Eject_Boost.Play
              If camY < -1000 Then '如果少于-1000单位是慢镜的
              VY = 2 * TV.TimeElapsed
              camY = -999
              End If
       GameTools.Eject_HasStartTime = 0
       GameTools.Eject_IsUsingSystem = False
       End Select
End If







'/////////////////////////////////////////////////////定时存钱
If inputE.IsKeyPressed(TV_KEY_RETURN) And GameTools.MoneySave_TheNumberPlayerHas > 0 And _
GameTools.MoneySave_IsUsing = False Then '按了ctrl，伤了血，拥有数大于零

              '音阶音效
              If GameSettings.IsSoundOn = True Then GameSound.GetTools.Play

              GameTools.MoneySave_IsUsing = True
              
              SetTextAlpha text_开启自动存钱系统, 1
              
              GameTools.MoneySave_TheNumberPlayerHas = GameTools.MoneySave_TheNumberPlayerHas - 1
              
              '重置冷却时间
              数据存档 (DataType_PlayerData)
              
End If

'开启了自动存钱
If GameTools.MoneySave_IsUsing = True Then

GameTools.MoneySave_HasSavedTime = GameTools.MoneySave_HasSavedTime + TV.TimeElapsed

       If GameTools.MoneySave_HasSavedTime > 10000 Then '每10秒存一次钱
       TotalCash = TotalCash + UnSavedCashInGame
       UnSavedCashInGame = 0
       GameTools.MoneySave_HasSavedTime = 0 '计时器归零
       End If
       
End If

End With 'TimeSlowMesh
















End Sub





















