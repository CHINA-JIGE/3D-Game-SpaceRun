Attribute VB_Name = "玩家"
Sub f移动系统()

inputE.GetMouseState tmpmouseX, tmpmouseY
inputE.GetMousePosition mouseX, mouseY

RotateX = RotateX + tmpmouseX * TV.TimeElapsed / 1000  '鼠标移动 镜头旋转
RotateY = RotateY - tmpmouseY * TV.TimeElapsed / 1000

Select Case RotateY
Case Is > 2
RotateY = 2
Case Is < -2
RotateY = -2
End Select

'If IsJumping = False Then '在空中不可以自由的移动呢 惯性嘛
   If inputE.IsKeyPressed(TV_KEY_W) = True Then
   VF = VF + TV.TimeElapsed / 6 * F阻力
   End If

   If inputE.IsKeyPressed(TV_KEY_S) = True Then
   VF = VF - TV.TimeElapsed / 6 * F阻力
   End If

   If inputE.IsKeyPressed(TV_KEY_A) = True Then
   VR = VR - TV.TimeElapsed / 6 * F阻力
   End If

   If inputE.IsKeyPressed(TV_KEY_D) = True Then
   VR = VR + TV.TimeElapsed / 6 * F阻力
   End If
'End If
'///////////////////////////////////////////////////////

Select Case VF '限制速度 &减速度
Case Is > TV.TimeElapsed * 1.5
VF = TV.TimeElapsed * 1.5
Case Is < -TV.TimeElapsed * 1.5
VF = -TV.TimeElapsed * 1.5
Case Is > 0
VF = VF - 0.2 * TV.TimeElapsed / 5 * F阻力
    If IsJumping = True Then VF = VF + 0.19 * TV.TimeElapsed / 5 * F阻力 '空中加的不要那么多啊
If VF < 0 Then VF = 0
Case Is < 0
VF = VF + 0.2 * TV.TimeElapsed / 5 * F阻力
    If IsJumping = True Then VF = VF - 0.19 * TV.TimeElapsed / 5 * F阻力
If VF > 0 Then VF = 0
End Select

Select Case VR '限制速度 &减速度
Case Is > TV.TimeElapsed * 1.5
VR = TV.TimeElapsed * 1.5
Case Is < -TV.TimeElapsed * 1.5
VR = -TV.TimeElapsed * 1.5
Case Is > 0
VR = VR - 0.2 * TV.TimeElapsed / 5 * F阻力
    If IsJumping = True Then VR = VR + 0.19 * TV.TimeElapsed / 5 * F阻力
If VR < 0 Then VR = 0
Case Is < 0
VR = VR + 0.2 * TV.TimeElapsed / 5 * F阻力
     If IsJumping = True Then VR = VR - 0.19 * TV.TimeElapsed / 5 * F阻力
If VR > 0 Then VR = 0
End Select

If GravityV < -10 Then GravityV = -10
'////////////////////////////////////////////////////////
'//////////////////////处理坐标/////////////////////////
f碰撞测试

CamLX = camX + 1000 * Sin(RotateX)
CamLY = camY + 1000 * Sin(RotateY)
CamLZ = camZ + 1000 * Cos(RotateX)


'////////////////////////跳跃及重力加速度//////////////////////
If IsJumping = True Then GravityV = GravityV - TV.TimeElapsed / 400 '可为负数
If IsJumping = False Then GravityV = 0

   If inputE.IsKeyPressed(TV_KEY_SPACE) = True And IsJumping = False Then
   IsJumping = True
   F阻力 = 0.1
   GravityV = 0.8
   tmpRotateX = RotateX
   tmpRotateY = RotateY
   End If
   
camY = camY + GravityV
cam.SetCamera camX, camY, camZ, CamLX, CamLY, CamLZ
Blood.SetGlobalPosition camX + 10, camY, camZ + 10
Blood.Update


HandX = camX
HandY = camY - 1
HandZ = camZ
hands.SetPosition HandX, HandY, HandZ
hands.LookAtPoint Vector(CamLX, CamLY, CamLZ), False

End Sub





Sub f碰撞测试()

With scene

If IsJumping = False Then '空中鼠标动了也不给你转向
tmpRotateX = RotateX
tmpRotateY = RotateY
End If


DeltaCamX = (VF * Sin(tmpRotateX) + VR * Cos(tmpRotateX)) * TV.TimeElapsed / 10   '向前和向右造成的绝对坐标系的X坐标变化
DeltaCamZ = (VF * Cos(tmpRotateX) - VR * Sin(tmpRotateX)) * TV.TimeElapsed / 10   '向前和向右造成的绝对坐标系的Z坐标变化

If .Collision(Vector(camX, camY, camZ), Vector(camX + (Sgn(DeltaCamX) - 0.5) * 15, camY, camZ)) = False And .Collision(Vector(camX + (Sgn(DeltaCamX) - 0.5) * 15, camY - 70, camZ), Vector(camX + (Sgn(DeltaCamX) - 0.5) * 15, camY - 70, camZ)) = False Then '下一步X 整个身体不撞
camX = camX + DeltaCamX 'sgn()正数是1，负数0 (Sgn(DeltaCamX) - 0.5) * 15大致说的是X坐标的增加，判断下一步的合理性
Else: camX = camX
End If

If .Collision(Vector(camX, camY, camZ), Vector(camX, camY, camZ + (Sgn(DeltaCamZ) - 0.5) * 15)) = False And .Collision(Vector(camX, camY, camZ + Sgn(DeltaCamZ) - 0.5 * 15), Vector(camX, camY - 70, camZ + (Sgn(DeltaCamZ) - 0.5) * 15)) = False Then '下一步X 整个身体不撞
camZ = camZ + DeltaCamZ
Else: camZ = camZ
End If



'////////////////////Y坐标的处理/////////////////////////////
If .Collision(Vector(camX, camY, camZ), Vector3(camX, camY - 100, camZ)) = True Then
IsJumping = False
    If .Collision(Vector(camX, camY, camZ), Vector(camX, camY - 99.5, camZ)) = True Then camY = camY + 0.5
    
    If GravityV < -3 Then '摔着了
    HP = HP - Abs(GravityV) * 10
    GE.Flash 1, 0, 0, 1000
    End If
    
    
F阻力 = 0.5
Else
IsJumping = True
F阻力 = 0.1
End If


   If .Collision(Vector(camX, camY, camZ), Vector(camX + (Sgn(DeltaCamX) - 0.5) * 15, camY - 110, camX + (Sgn(DeltaCamZ) - 0.5) * 15)) = False Then
   F阻力 = 0.5
   End If


End With

End Sub



Sub f攻击系统()
If inputE.IsMouseButtonPressed(0) = True And HandIsPlaying = False Then
HandIsPlaying = True
hands.SetAnimationByName ("left")
hands.PlayAnimation 3
End If

If inputE.IsMouseButtonPressed(1) = True And HandIsPlaying = False Then
HandIsPlaying = True
hands.SetAnimationByName ("right")
hands.PlayAnimation 3
End If


If hands.IsAnimationFinished = True Then
hands.StopAnimation
HandIsPlaying = False
End If

End Sub








