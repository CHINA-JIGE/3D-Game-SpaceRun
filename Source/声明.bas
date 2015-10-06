Attribute VB_Name = "声明"
Public Declare Function AddFontResource Lib "gdi32" Alias "AddFontResourceA" (ByVal lpFileName As String) As Long
Public Declare Function GetDC Lib "user32" (ByVal hwnd As Long) As Long
'截屏
Public Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal X As Long, ByVal Y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long


Public Shader As TVShader




Enum ButtonStatus
Button_MouseNotOn = 1
Button_MouseOn = 2
Button_IsClicked = 3
End Enum




'////////////////////////////////////////声音
'////////////////////////////////////////声音
'////////////////////////////////////////声音
'////////////////////////////////////////声音
Public SOUNDE As TVSoundEngine
Public GameBGM(1 To 3) As TVSoundMP3
Public MenuBGM              As TVSoundMP3
Public GameBGMID            As Integer
'Public CoinSoundID          As Integer
'Public CoinSoundCountTime          As Single

Public Type GameSoundType
Explode(1 To 30)                            As TVSoundMP3
Engine                              As TVSoundMP3
Coin(1 To 70)                               As TVSoundMP3
Scream                             As TVSoundMP3
HeartBeat                          As TVSoundMP3
Eject_Charge                       As TVSoundMP3
Eject_Boost                           As TVSoundMP3
GetTools                                As TVSoundMP3
End Type
Public GameSound As GameSoundType






'///////////////////////////////////////////特殊事件
Public SE                    As SpecialEvent

Enum SpecialEventID '特殊事件ID
SpecialEvent_NoGravityArea = 1 '无重力区
SpecialEvent_RockFall = 2  '陨石群
SpecialEvent_SpeedTunnel = 3  '加减速隧道
SpecialEvent_Gold = 4 '金陨石
'还有那个移动石块占了5
End Enum

Public Type SpecialEvent
EventID                      As SpecialEventID
EventReBoostTime             As Single
EventCountTime               As Single
EventActivated               As Boolean
End Type
'////////////////////////////////////////////////






'//////////////////////////////////////////////////////////////////
Public FR(1 To 30)           As FallingRock '坠落的陨石
Public ExplosionAnimation(1 To 30)    As TVMesh
Public FRHasFallNumber       As Single '落石已落下的数目
Public FallingRock(1 To 30)  As TVMesh
Public Type FallingRock

FR_Scale                     As Single
X                            As Single
Y                            As Single
Z                            As Single
StartX                       As Single
StartY                       As Single
StartZ                       As Single
Mesh                         As TVMesh
End Type

'//////////////////////////////////////////////








'/////////////////////////'那些浮着的陨石
Public Asteroid(1 To 30)     As TVMesh
Public AstMP(1 To 30)        As AsteroidMoveParam 'Asteroid Move Parameters

Public Type AsteroidMoveParam
MX                           As Single
MY                           As Single
MZ                           As Single
RX                           As Single 'rotateX
RY                           As Single
RZ                           As Single
AbsRX                        As Single 'Absolute Rotate X
AbsRY                        As Single
AbsRZ                        As Single
End Type

'//////////////////////////////////////////////






'/////////////////////////////////////////人民币= =
Public RMB(1 To 70)         As TVMesh
Public RmbPosZ(1 To 70)      As Single
Public RmbPosX(1 To 70)      As Single
Public RmbIsMagnetic(1 To 70) As Boolean

Enum RMMode '金币重置模式
ResetMode_Normal = 0
ResetMode_Random = 1
End Enum
'//////////////////////////////////////////////










'/////////////////////////////////////////////////
Public Type UIButton
MouseIsOn                   As Boolean
MouseHasClick               As Boolean
LeftTop_X1                  As Single
LeftTop_Y1                  As Single
Width                              As Single
Height                             As Single
MouseStatus                 As ButtonStatus
End Type
'/////////////////////////////////////////////////









'////////////////////////////////淡出的文字
Public Type UIScrTextAlpha
FadeOutSpeed                As Single '淡出的速度
Alpha                       As Single
End Type

'//////////////////画一个淡出文字有4步
'////////////////1、枚举类加多一个项目
'///////////////2、初始化设定淡出速度
'///////////////3、触发事件时设定ALPHA为1
'//////////////4、绘图2D那里画图

Public Enum ScrTextName
Text_小金块 = 1
Text_人民币50 = 2
Text_人民币1000 = 3
Text_金块奖励模式 = 4
Text_无重力模式 = 5
text_撞到陨石 = 6
text_摔着了 = 7
text_烧着了 = 8
text_小心陨石 = 9
text_变速隧道出现 = 10
text_石块会动 = 11
text_拿到磁铁 = 12
text_存钱 = 13
text_满血 = 14
text_道具未冷却 = 15
text_开启自动存钱系统 = 16
text_HP已满 = 17
text_滞空时间加成 = 18
text_时间流逝速度 = 19
text_已截屏 = 20
End Enum













Public Playlevel             As Integer '设置玩的场景 用case
Public NextPlayLevelToGo  As Integer '一开始菜单 是否淡出完毕然后可以继续
Public PlayLevelBeforeSetting        As Integer
Public HP                    As Single '玩家血量
Public GameScore             As Double '游戏分数
Public HighScore               As Double


Public TV                    As New TVEngine
Public inputE                As New TVInputEngine
Public scene                 As New TVScene
Public cam                   As New TVCamera
Public texF                  As New TVTextureFactory
Public atmo                  As New TVAtmosphere
Public LE                    As New TVLightEngine
Public MatF                  As New TVMaterialFactory
Public GE                    As New TVGraphicEffect
Public SCR                   As New TVScreen2DImmediate
Public SCRText               As New TVScreen2DText
Public GlowRS                As New TVRenderSurface '泛光图层
Public DeepRS                As New TVRenderSurface '景深



 '//////////////////////////NormalRunway普通跑道
Public NRWay(1 To 15)        As TVMesh
Public NRWPosZ(1 To 15)      As Single
Public NRWPosY(1 To 15)      As Single

'///////////////////////粒子系统
Public LittleRock            As TVParticleSystem
Public PickMoneyParticle(1 To 70) As TVParticleSystem
Public ExplosionParticle(1 To 30) As TVParticleSystem '爆炸的时候



Public NoGravityArea         As TVParticleSystem
Public NGALaser              As TVMesh

'Public NGAColor              As TV_COLOR '无重力区的Color

'////////////////////////////变速隧道
Public StartTunnel           As TVMesh '开始隧道
Public SpeedUpTunnel         As TVMesh '加速隧道
Public SpeedDownTunnel       As TVMesh '减速隧道
Public SpeedTunnelRing(1 To 2) As TVActor '转动环
Public SpeedUpParticle      As TVParticleSystem
Public SpeedDownParticle    As TVParticleSystem



'///////////////////////////储存Money跑道
Public MoneySaveRunway      As TVMesh
Public MoneyMultiplier      As Integer
Public HaveSavedMoney       As Boolean
Public UnSavedCashInGame    As Long '没存的money
Public TotalCash            As Long




'///////////////////////////游戏道具
Public Type GameToolsType
Magnet_IsPlayerHas                    As Boolean
Magnet_OnHandTime                 As Single
Magnet_Mesh                               As TVMesh
Eject_TheNumberPlayerHas       As Integer
Eject_IsUsingSystem                     As Boolean
Eject_Step                                       As Integer
Eject_HasStartTime                        As Single
MoneySave_TheNumberPlayerHas   As Integer
MoneySave_IsUsing                         As Boolean
MoneySave_HasSavedTime             As Single '离上次存钱已过的时间
HP_InShop_TheNumberPlayerHas   As Integer
RecoverHP_Mesh                            As TVMesh
TimeSlow_IsPlayerHas                      As Boolean
TimeSlow_OnHandTime                As Single
TimeSlow_Mesh                             As TVMesh
End Type


Public GameTools As GameToolsType '实例化









'////////////////////////////////////////

Public Vcam                  As Single '速度
Public Scam                  As Single '路程
Public camX                  As Single
Public camY                  As Single
Public camZ                  As Single
Public CamLX                 As Single
Public CamLY                 As Single
Public CamLZ                 As Single
Public DeltaCamZ             As Single
Public DeltaCamY             As Single
Public VY                    As Single '纵向速度
Public F阻力                 As Single
Public DeltaVConst           As Single '速度常数 平衡FPS不同造成的误差
Public StayInAirTime        As Single '滞空时间 用于加成
Public StayInAirBonus       As Long

Public FormCenterX           As Single '窗体中心点X
Public FormCenterY           As Single 'Y
Public FormHeight            As Single
Public FormWidth             As Single

'Public FRColRes(1 To 30)          As TV_COLLISIONRESULT



Public VR                    As Single 'Right左右
Public RotateX               As Single
Public RotateY               As Single
Public IsDead                As Boolean
Public SunSize               As Single '那个大星球的大小
Public RMBRotateY            As Single
Public tmpmouseX             As Long '
Public tmpmouseY             As Long
Public MouseX                As Long '鼠标的坐标 像素
Public MouseY                As Long

Public HasJumped             As Integer
Public CanPressW             As Boolean
Public HasPressWTime         As Single '离上一次按过了多久




'//////////////////////UI和文字
Public ScrTextAlpha(1 To 20)        As UIScrTextAlpha '预留20个空位先
Public Button(1 To 20)               As UIButton '预留15个空位





'.////////////////////////////////////////////功能函数////////////////////////
'.////////////////////////////////////////////////////////////////////
'.////////////////////////////////////////////////////////////////////
'.////////////////////////////////////////////////////////////////////
'
Public N_X                   As Single
Public N_Y                   As Single
Public N_Z                   As Single





'////////////////MoveCam
Public CamMoveX              As Single
Public CamMoveY              As Single
Public CamMoveZ              As Single
Public CamPosMoveFinish      As Boolean
Public CamLookatMoveFinish   As Boolean
Public CamMoveLX             As Single
Public CamMoveLY             As Single
Public CamMoveLZ             As Single
Public CamViewDegree         As Single





'///////////////震屏
Public ShockTime             As Single
Public ShockTimeOnce         As Single
Public ShockActivated        As Boolean




'//////////////////爆炸动画
Public Type ExplosionAnimationType
CountTime                   As Single
TextureID                   As Integer '只是在这个序列当中的ID
Activated                   As Boolean
End Type
Public ExpAnim(1 To 30)     As ExplosionAnimationType


'/////////////////////特殊事件 speedTunnel
Public SpeedTunnelX         As Single


'/////////////////////特殊事件 NoGravityArea
Public NoGravityActivated   As Boolean


'/////////////////////特殊事件 GOLD
Public Gold_BonusCash            As Single
Public SE_Gold_LastTime     As Single 'BONUS模式开了多久


'////////////////////////特殊事件 石块移动
Public RockHasMove_Time          As Single '石块开始移了多久
Public RockHasMove_Rounds        As Single '石块移了多少个来回
Public TmpNRWayPosZ(1 To 15)     As Single
Public MoveDifferenceConst(1 To 15) As Single '使他们的周期起始不同







'/////////////////////////绘图 2D标记 用来储存转换出来的2D点
Public MoneySave2DPointX As Single, MoneySave2DPointY As Single
Public SpeedUpDPointX As Single, SpeedUp2DPointY      As Single
Public SpeedDownDPointX As Single, SpeedDown2DPointY  As Single
Public Fr2DX(1 To 30) As Single, Fr2DY(1 To 30)       As Single


'//////////////////////////绘图2D 评分屏
Public DieMenuCountTime                     As Single '计时爆字


'////////////////////////////绘图2D 商店
Public AfterPressCountTime                  As Single










'//////////////////存档
Public EnCryptionFlag As Integer
Public DeCryptionFlag As Integer

Enum DataInputSaveMode
DataType_SettingData = 1 '设置的数据
DataType_PlayerData = 2 '玩家数据
End Enum




'////////////////////设置参数
Public Type GameSettingsType
MouseSensitivity As Single
IsSoundOn        As Integer
End Type
Public GameSettings As GameSettingsType






'//////////////////////特殊按键与道具使用
Public Type AfterPressKeyCountTime
Key_Space_Time       As Single
Key_LeftCtrl_Time    As Single
Key_PrintScreen      As Single
Key_PageDown          As Single
End Type

Public KeyCountTime As AfterPressKeyCountTime



'/////////////////////游戏速度系数 用于慢镜
Public TimePassSpeed        As Single '默认值为1
